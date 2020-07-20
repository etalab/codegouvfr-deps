;; Copyright (c) 2020 DINUM, Bastien Guerry <bastien.guerry@data.gouv.fr>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSE

(ns core
  (:require  [cheshire.core :as json]
             [babashka.curl :as curl]
             [clojure.data.xml :as xml]
             [clojure.string :as s]
             [clojure.edn :as edn]
             [clojure.walk :as walk]
             [hickory.core :as h]
             [hickory.select :as hs]
             [java-time :as t])
  (:gen-class))

;; Definitions

(defonce dep-files
  {"PHP"        ["composer.json"]
   "Vue"        ["package.json"]
   "JavaScript" ["package.json"]
   "TypeScript" ["package.json"]
   "Python"     ["setup.py" "requirements.txt"]
   "Ruby"       ["Gemfile"]
   "Java"       ["pom.xml"]
   "Clojure"    ["pom.xml" "deps.edn" "project.clj"]})

(defonce repos-url
  "https://raw.githubusercontent.com/etalab/data-codes-sources-fr/master/data/repertoires/json/all.json")

(def repos
  (when-let [res (or (try (slurp "repos-raw.json")
                          (catch Exception e
                            (println (.getMessage e))))
                     (:body (try (curl/get repos-url)
                                 (catch Exception e
                                   (println (.getMessage e))))))]
    (atom (json/parse-string res true))))

(def reused-init
  (when-let [res (try (slurp "reuse.json")
                      (catch Exception e
                        (println (.getMessage e))))]
    (json/parse-string res)))

(def deps-init
  (let [deps (try (slurp "deps.json")
                  (catch Exception e
                    (println (.getMessage e))))]
    (atom (json/parse-string deps true))))

;; Utility functions

(defn- less-than-x-days-ago [days date-str]
  (let [x-days-ago (t/minus (t/instant) (t/days days))]
    (= (t/min (t/instant date-str) x-days-ago) x-days-ago)))

(defn- check-module-of-type-is-known [module type]
  (when-let [res (not-empty
                  (filter #(and (= module (:name %)) (= type (:type %)))
                          @deps-init))]
    (when (less-than-x-days-ago 28 (:updated (first res)))
      (first res))))

;; Module validation

(defn- get-valid-npm [module]
  (or
   (check-module-of-type-is-known module "npm")
   (let [registry-url-fmt "https://registry.npmjs.org/-/v1/search?text=%s&size=1"]
     (when-let [res (try (curl/get (format registry-url-fmt module))
                         (catch Exception _ nil))]
       (let [{:keys [description links]}
             (-> (:body res)
                 (json/parse-string true)
                 :objects first :package)]
         {:name        module
          :type        "npm"
          :updated     (str (t/instant))
          :description description
          :link        (:npm links)})))))

(defn- get-valid-pypi [module]
  (or
   (check-module-of-type-is-known module "pypi")
   (let [registry-url-fmt "https://pypi.org/pypi/%s/json"]
     (when-let [res (try (curl/get (format registry-url-fmt module))
                         (catch Exception _ nil))]
       (let [{:keys [info]}
             (-> (:body res)
                 (json/parse-string true))]
         {:name        module
          :type        "pypi"
          :updated     (str (t/instant))
          :description (:summary info)
          :link        (:package_url info)})))))

;; FIXME: Where to get a proper maven artifact description?
(defn- get-valid-maven [module]
  (or
   (check-module-of-type-is-known module "maven")
   (let [[groupId artifactId] (drop 1 (re-find #"([^/]+)/([^/]+)" module))
         registry-url-fmt
         "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&core=gav&rows=1&wt=json"
         link-fmt
         "https://search.maven.org/classic/#search|ga|1|g:%%22%s%%22%%20AND%%20a:%%22%s%%22"]
     (when-let [res (try (curl/get (format registry-url-fmt groupId artifactId))
                         (catch Exception _ nil))]
       (when-let [tags (not-empty (-> (json/parse-string (:body res) true)
                                      :response
                                      :docs
                                      first
                                      :tags))]
         {:name        module
          :type        "maven"
          :updated     (str (t/instant))
          :description (s/join ", " (take 6 tags))
          :link        (format link-fmt groupId artifactId)})))))

(defn- get-valid-clojars [module]
  (or
   (check-module-of-type-is-known module "clojure")
   (let [registry-url-fmt "https://clojars.org/api/artifacts/%s"]
     (when-let [res (try (curl/get (format registry-url-fmt module))
                         (catch Exception _ nil))]
       {:name        module
        :type        "clojure"
        :updated     (str (t/instant))
        :description (:description (json/parse-string (:body res) true))
        :link        (str "https://clojars.org/" module)}))))

(defn- get-valid-bundler [module]
  (or
   (check-module-of-type-is-known module "bundler")
   (let [registry-url-fmt "https://rubygems.org/api/v1/gems/%s.json"]
     (when-let [res (try (curl/get (format registry-url-fmt module))
                         (catch Exception _ nil))]
       (let [{:keys [info project_uri]} (json/parse-string (:body res) true)]
         {:name        module
          :type        "bundler"
          :updated     (str (t/instant))
          :description info
          :link        project_uri})))))

(defn- get-valid-composer [module]
  (or
   (check-module-of-type-is-known module "composer")
   (let [registry-url-fmt "https://packagist.org/packages/%s"]
     (when-let [res (try (curl/get (str (format registry-url-fmt module) ".json"))
                         (catch Exception _ nil))]
       {:name        module
        :type        "composer"
        :updated     (str (t/instant))
        :description (:description (:package (json/parse-string (:body res) true)))
        :link        (format registry-url-fmt module)}))))

;; Reuse information

(defn- get-reuse
  "Return a hash-map with reuse information"
  [repertoire_url]
  (when-let [repo-github-html
             (try (curl/get (str repertoire_url "/network/dependents"))
                  (catch Exception e
                    (println (.getMessage e))))]
    (let [btn-links (-> repo-github-html
                        :body
                        h/parse
                        h/as-hickory
                        (as-> d (hs/select (hs/class "btn-link") d)))
          nb-reps   (or (try (re-find #"\d+" (last (:content (nth btn-links 1))))
                             (catch Exception _ "0")) "0")
          nb-pkgs   (or (try (re-find #"\d+" (last (:content (nth btn-links 2))))
                             (catch Exception _ "0")) "0")]
      (hash-map
       repertoire_url
       {:updated (str (t/instant))
        :reuse   (+ (edn/read-string nb-reps)
                    (edn/read-string nb-pkgs))}))))

(defn- add-reuse
  "Return a hash-map entry with the repo URL and the reuse information."
  [{:keys [repertoire_url]}]
  (if-let [{:keys [updated] :as entry}
           (walk/keywordize-keys
            (get reused-init repertoire_url))]
    (if (less-than-x-days-ago 14 updated)
      (hash-map repertoire_url entry)
      (get-reuse repertoire_url))
    (get-reuse repertoire_url)))

(defn- spit-reuse-info
  "Generate reuse.json with GitHub reused-by information."
  []
  (let [res (atom {})]
    (doseq [r (filter #(= (:plateforme %) "GitHub") @repos)]
      (when-let [info (add-reuse r)]
        (swap! res conj info)))
    (spit "reuse.json" (json/generate-string @res)))
  (println "Added reuse information and stored it in reuse.json"))

;; Dependencies information

(defn- get-packagejson-deps [body]
  (let [parsed (json/parse-string body)
        deps   (get parsed "dependencies")]
    (when (seq deps)
      {:npm (into [] (keys deps))})))

(defn- get-composerjson-deps [body]
(let [parsed (json/parse-string body)
      deps   (get parsed "require")]
  (when (seq deps)
    {:composer (into [] (keys deps))})))

(defn- get-setuppy-deps [body]
(let [deps0 (last (re-find #"(?ms)install_requires=\[([^]]+)\]" body))]
  (when (seq deps0)
    (let [deps (map #(get % 1) (re-seq #"'([^>\n]+)(>=.+)?'" deps0))]
      (when (seq deps)
        {:pypi (into [] (map s/trim deps))})))))

(defn- get-requirements-deps [body]
(when (not-empty body)
  (let [deps (map last (re-seq #"(?m)^([^=]+)==.+" body))]
    (when (seq deps)
      {:pypi (into [] (map s/trim deps))}))))

(defn- get-gemfile-deps [body]
(let [deps (re-seq #"(?ms)^gem '([^']+)'" body)]
  (when (seq deps)
    {:bundler (into [] (map last deps))})))

(defn- get-depsedn-deps [body]
(let [deps (->> (map first (:deps (edn/read-string body)))
                (map str)
                (filter #(not (re-find #"^org\.clojure" %)))
                (map symbol)
                (map name))]
  (when deps {:clojars (into [] deps)})))

(defn- get-projectclj-deps [body]
(let [deps (->> (edn/read-string body)
                (drop 3)
                (apply hash-map)
                :dependencies
                (map first)
                (filter #(not (re-find #"^org\.clojure" (name %))))
                (map name))]
  (when deps {:clojars (into [] deps)})))

(defn- get-pomxml-deps [body]
  (when-let [deps0 (try (filter #(= (name (:tag %)) "dependencies")
                                (->> (:content (xml/parse-str body))
                                     (remove string?)))
                        (catch Exception e (println (.getMessage e))))]
    (let [deps (->> deps0 first :content
                    (remove string?)
                    (map #(let [[g a] (remove string? (:content %))]
                            (str (first (:content g)) "/"
                                 (first (:content a)))))
                    (remove nil?)
                    flatten)]
      (when (seq deps)
        {:maven (into [] deps)}))))

(defn- add-dependencies
  "Take a repository map and return the map completed with dependencies."
  [{:keys
    [repertoire_url organisation_nom est_archive
     nom plateforme langage deps_updated] :as repo}]
  (if (or (= langage "")
          (= est_archive true)
          (when-let [d (not-empty deps_updated)]
            (less-than-x-days-ago 14 d)))
    repo
    (let [baseurl    (re-find #"https?://[^/]+" repertoire_url)
          fmt-str    (if (= plateforme "GitHub")
                       "https://raw.githubusercontent.com/%s/%s/master/%s"
                       (str baseurl "/%s/%s/-/raw/master/%s"))
          dep-fnames (get dep-files langage)
          deps       (atom {})]
      (doseq [f dep-fnames]
        (when-let [res (try (curl/get (format fmt-str organisation_nom nom f))
                            (catch Exception e (println (.getMessage e))))]
          (when (= 200 (:status res))
            (let [body (:body res)
                  reqs (condp = f
                         "package.json"
                         (get-packagejson-deps body)
                         "composer.json"
                         (get-composerjson-deps body)
                         "setup.py"
                         (get-setuppy-deps body)
                         "requirements.txt"
                         (get-requirements-deps body)
                         "Gemfile"
                         (get-gemfile-deps body)
                         "deps.edn"
                         (get-depsedn-deps body)
                         "project.clj"
                         (get-projectclj-deps body)
                         "pom.xml"
                         (get-pomxml-deps body))]
              (swap! deps #(merge-with into % reqs))))))
      (assoc repo :deps @deps :deps_updated (str (t/instant))))))

(defn- deps-map-to-valid-deps-list [[k v]]
  (let [deps-list (map #(hash-map :type (name k) :name %) v)]
    (-> (map #(->> (filter (fn [{:keys [name type]}]
                             (and (= name (:name %))
                                  (= type (:type %)))) @deps-init)
                   (map (fn [e] (dissoc e :updated))))
             deps-list)
        flatten)))

(defn- spit-repos-deps []
  (let [res (map
             (fn [r]
               (update r :deps
                       #(into [] (flatten (map deps-map-to-valid-deps-list %)))))
             @repos)]
    (reset! repos res)
    (spit "repos-deps.json" (json/generate-string res))
    (println "Added repos-deps.json")))

(defn- update-repos-deps-raw
  "Update @repos with dependencies information."
  []
  (let [res (atom [])]
    (doseq [r @repos]
      (let [deps (add-dependencies r)]
        (swap! res conj deps)))
    (reset! repos @res))
  ;; (spit "repos-deps.json" (json/generate-string @repos))
  ;; (println "Updated repos-deps.json")
  )

(defn- update-deps
  "Update @deps-init with the list of valid dependencies."
  []
  (when-let [deps (not-empty (filter not-empty (map :deps @repos)))]
    (let [d   (reduce #(merge-with into %1 %2) deps)
          res (atom {})]
      (doseq [[type modules] d]
        (swap! res concat
               (->>
                (condp = type
                  :npm      (map get-valid-npm modules)
                  :bundler  (map get-valid-bundler modules)
                  :maven    (map get-valid-maven modules)
                  :clojars  (map get-valid-clojars modules)
                  :composer (map get-valid-composer modules)
                  :pypi     (map get-valid-pypi modules))
                (remove nil?))))
      (reset! deps-init @res)
      ;; (spit "deps.json" (json/generate-string @res))
      )
    ;; (println "Added deps.json")
    ))

(defn- spit-deps-with-repos []
  (let [reps (map #(select-keys % [:deps :repertoire_url]) @repos)
        deps-reps
        (map (fn [{:keys [name type] :as dep}]
               (->> (map :repertoire_url
                         (filter (fn [{:keys [deps]}]
                                   (not-empty
                                    (filter (fn [d] (and (= (:name d) name)
                                                         (= (:type d) type)))
                                            deps)))
                                 reps))
                    (assoc dep :repos)))
             @deps-init)]
    (reset! deps-init deps-reps)
    (spit "deps.json" (json/generate-string deps-reps))))

(defn- spit-deps-repos []
  (let [reps0 (group-by (juxt :nom :organisation_nom) @repos)
        reps  (reduce-kv (fn [m k v] (assoc m k (first (map :deps v))))
                         {}
                         reps0)]
    (spit "deps-repos.json"
          (json/generate-string reps))
    (println "Updated deps-repos.json")))

(defn- spit-deps-orgas []
  (let [orgs0 (group-by (juxt :organisation_nom :plateforme) @repos)
        orgs  (reduce-kv (fn [m k v] (assoc m k (flatten (map :deps v))))
                         {}
                         orgs0)]
    (spit "deps-orgas.json" (json/generate-string orgs))
    (println "Added deps-orgas.json")))

(defn- spit-deps-total []
  (spit "deps-total.json"
        (json/generate-string
         {:deps-total (count @deps-init)}))
  (println "Added deps-total.json"))

(defn- spit-deps-top []
  (spit "deps-top.json"
        (json/generate-string
         (->> @deps-init
              (sort-by #(count (:repos %)))
              reverse
              (take 100))))
  (println "Added deps-top.json"))

(defn -main []
  (spit-reuse-info)        ;; reuse.json
  (update-repos-deps-raw)  ;; update @repos with deps raw
  (update-deps)            ;; update @deps-init
  (spit-repos-deps)        ;; repos-deps.json
  (spit-deps-with-repos)   ;; deps.json
  (spit-deps-repos)        ;; deps-repos.json
  (spit-deps-orgas)        ;; deps-orgas.json
  (spit-deps-total)        ;; deps-total.json
  (spit-deps-top)          ;; deps-top.json
  (println "Added all json files"))

;; (-main)
