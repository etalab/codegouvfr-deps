;; Copyright (c) 2020 DINUM, Bastien Guerry <bastien.guerry@data.gouv.fr>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSE

(ns core
  (:require  [jsonista.core :as json]
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

(defn- json-parse-with-keywords [s]
  (json/read-value s (json/object-mapper {:decode-key-fn keyword})))

;; Initialize repos-deps.
(def repos-deps
  (group-by (juxt :nom :organisation_nom)
            (json-parse-with-keywords
             (try (slurp "repos-deps.json")
                  (catch Exception _ nil)))))

(defn- find-first-matching-repo [{:keys [nom organisation_nom]}]
  (-> (get repos-deps [nom organisation_nom])
      first
      (select-keys [:deps_updated :deps])))

;; Initialize @repos by reusing :deps_updated and :deps from
;; repos-deps.json when available, otherwise using repos-raw.json,
;; falling back on curl'ing repos-url if needed.
(def repos
  (let [res (-> (or (try (slurp "repos-raw.json")
                         (catch Exception e
                           (println (.getMessage e))))
                    (:body (try (curl/get repos-url)
                                (catch Exception e
                                  (println (.getMessage e))))))
                json-parse-with-keywords)]
    (atom (map #(merge % (find-first-matching-repo %)) res))))

;; Initialize reused.
(def reused
  (when-let [res (try (slurp "reuse.json")
                      (catch Exception e
                        (println (.getMessage e))))]
    (json/read-value res)))

;; Initialize @deps.
(def deps
  (let [deps (try (slurp "deps.json")
                  (catch Exception e
                    (println (.getMessage e))))]
    (atom (->> (distinct (json-parse-with-keywords deps))
               (map #(dissoc % :repos))))))

(def grouped-deps
  (atom (group-by (juxt :name :type) @deps)))

;; Utility functions

(defn- less-than-x-days-ago [days date-str]
  (let [x-days-ago (t/minus (t/instant) (t/days days))]
    (= (t/min (t/instant date-str) x-days-ago) x-days-ago)))

(defn- check-module-of-type-is-known [module type]
  (when-let [res (-> (get @grouped-deps [module type])
                     first not-empty)]
    (when (less-than-x-days-ago 28 (:updated res)) res)))

(defn- flatten-deps [m]
  (let [t (str (t/instant))]
    (-> (fn [[k v]] (map #(assoc {} :type (name k) :name % :updated t) v))
        (map m)
        flatten
        distinct)))

(defn- find-first-matching-module [{:keys [name type]}]
  (-> (get @grouped-deps [name type])
      first
      (dissoc :repos)))

;; Module validation

(defn- get-valid-npm [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "npm")
   (do
     (println "Fetch info for npm module" name)
     (let [registry-url-fmt "https://registry.npmjs.org/-/v1/search?text=%s&size=1"]
       (when-let [res (try (curl/get (format registry-url-fmt name))
                           (catch Exception _ nil))]
         (when (= (:status res) 200)
           (let [{:keys [description links]}
                 (-> (try (json-parse-with-keywords (:body res))
                          (catch Exception _ nil))
                     :objects first :package)]
             {:name        name
              :type        "npm"
              :updated     (str (t/instant))
              :description description
              :link        (:npm links)})))))))

(defn- get-valid-pypi [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "pypi")
   (do
     (println "Fetch info for pypi module" name)
     (let [registry-url-fmt "https://pypi.org/pypi/%s/json"]
       (when-let [res (try (curl/get (format registry-url-fmt name))
                           (catch Exception _ nil))]
         (when-let [{:keys [info]}
                    (try (json-parse-with-keywords (:body res))
                         (catch Exception _ nil))]
           {:name        name
            :type        "pypi"
            :updated     (str (t/instant))
            :description (:summary info)
            :link        (:package_url info)}))))))

;; FIXME: Where to get a proper maven artifact description?
(defn- get-valid-maven [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "maven")
   (do
     (println "Fetch info for maven module" name)
     (let [[groupId artifactId] (drop 1 (re-find #"([^/]+)/([^/]+)" name))
           registry-url-fmt
           "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&core=gav&rows=1&wt=json"
           link-fmt
           "https://search.maven.org/classic/#search|ga|1|g:%%22%s%%22%%20AND%%20a:%%22%s%%22"]
       (when-let [res (try (curl/get (format registry-url-fmt groupId artifactId))
                           (catch Exception _ nil))]
         (when-let [tags (not-empty
                          (-> (try (json-parse-with-keywords (:body res))
                                   (catch Exception _ nil))
                              :response
                              :docs
                              first
                              :tags))]
           {:name        name
            :type        "maven"
            :updated     (str (t/instant))
            :description (s/join ", " (take 6 tags))
            :link        (format link-fmt groupId artifactId)}))))))

(defn- get-valid-clojars [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "clojars")
   (do
     (println "Fetch info for clojars module" name)
     (let [registry-url-fmt "https://clojars.org/api/artifacts/%s"]
       (when-let [res (try (curl/get (format registry-url-fmt name))
                           (catch Exception _ nil))]
         (when (= (:status res) 200)
           {:name        name
            :type        "clojars"
            :updated     (str (t/instant))
            :description (:description
                          (try (json-parse-with-keywords (:body res))
                               (catch Exception _ nil)))
            :link        (str "https://clojars.org/" name)}))))))

(defn- get-valid-bundler [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "bundler")
   (do
     (println "Fetch info for bundler module" name)
     (let [registry-url-fmt "https://rubygems.org/api/v1/gems/%s.json"]
       (when-let [res (try (curl/get (format registry-url-fmt name))
                           (catch Exception _ nil))]
         (when (= (:status res) 200)
           (let [{:keys [info project_uri]}
                 (try (json-parse-with-keywords (:body res))
                      (catch Exception _ nil))]
             {:name        name
              :type        "bundler"
              :updated     (str (t/instant))
              :description info
              :link        project_uri})))))))

(defn- get-valid-composer [{:keys [name]}]
  (or
   (check-module-of-type-is-known name "composer")
   (do
     (println "Fetch info for composer module" name)
     (let [registry-url-fmt "https://packagist.org/packages/%s"]
       (when-let [res (try (curl/get (str (format registry-url-fmt name) ".json"))
                           (catch Exception _ nil))]
         (when (= (:status res) 200)
           {:name        name
            :type        "composer"
            :updated     (str (t/instant))
            :description (-> (try (json-parse-with-keywords (:body res))
                                  (catch Exception _ nil))
                             :package
                             :description)
            :link        (format registry-url-fmt name)}))))))

;; Reuse information

(defn- get-reuse
  "Return a hash-map with reuse information"
  [repertoire_url]
  (when-let [repo-github-html
             (try (curl/get (str repertoire_url "/network/dependents"))
                  (catch Exception _ nil))]
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
            (get reused repertoire_url))]
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
    (spit "reuse.json" (json/write-value-as-string (merge reused @res))))
  (println "Added reuse information and stored it in reuse.json"))

;; Dependencies information

(defn- get-packagejson-deps [body]
  (let [parsed (json/read-value body)
        deps   (get parsed "dependencies")]
    (when (seq deps)
      {:npm (into [] (keys deps))})))

(defn- get-composerjson-deps [body]
  (let [parsed (json/read-value body)
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
  (when-let [deps0 (try (not-empty
                         (filter #(= (name (:tag %)) "dependencies")
                                 (->> (:content (xml/parse-str body))
                                      (remove string?))))
                        (catch Exception _ nil))]
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
                            (catch Exception _ nil))]
          (println "Fetching dependencies for" (format fmt-str organisation_nom nom f))
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
      (assoc repo :deps (flatten-deps @deps) :deps_updated (str (t/instant))))))

(defn- update-repos-deps
  "Update @repos with dependencies information."
  []
  (reset! repos (map add-dependencies @repos))
  (println "Updated @repos with dependencies"))

(defn- spit-repos-deps
  "Add valid :deps to each repo and spit repos-deps.json."
  []
  (reset! repos (map (fn [r]
                       (update
                        r :deps
                        #(map find-first-matching-module %)))
                     @repos))
  (spit "repos-deps.json" (json/write-value-as-string @repos))
  (println "Added or updated repos-deps.json"))

(defn- validate-repos-deps
  "Update @deps with the list of valid dependencies."
  []
  (when-let [d (-> (group-by :type (->> (map :deps @repos)
                                        flatten
                                        (remove nil?)))
                   walk/keywordize-keys
                   not-empty)]
    (let [res (atom {})]
      (doseq [[type modules] d]
        (swap! res concat
               (->>
                (condp = type
                  :npm      (map get-valid-npm modules)
                  :bundler  (map get-valid-bundler modules)
                  :maven    (map get-valid-maven modules)
                  :clojars  (map get-valid-clojars modules)
                  :composer (map get-valid-composer modules)
                  :pypi     (map get-valid-pypi modules)
                  nil)
                (remove nil?))))
      (reset! deps (distinct @res))
      (reset! grouped-deps (group-by (juxt :name :type) @deps))
      (println "Updated @deps with valid dependencies"))))

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
             @deps)]
    (reset! deps (distinct deps-reps))
    (spit "deps.json" (json/write-value-as-string deps-reps))
    (println "Added or updated deps.json")))

(defn- get-all-deps [m]
  (distinct (map #(dissoc % :updated) (flatten (map :deps m)))))

(defn- spit-deps-repos []
  (let [reps0 (group-by (juxt :nom :organisation_nom) @repos)
        reps  (reduce-kv (fn [m k v] (assoc m k (get-all-deps v)))
                         {}
                         reps0)]
    (spit "deps-repos.json"
          (json/write-value-as-string reps))
    (println "Added deps-repos.json")))

(defn- spit-deps-orgas []
  (let [orgs1 (group-by (juxt :organisation_nom :plateforme) @repos)
        orgs0 (reduce-kv (fn [m k v] (assoc m k (get-all-deps v)))
                         {}
                         orgs1)]
    (spit "deps-orgas.json" (json/write-value-as-string orgs0))
    (println "Added deps-orgas.json")))

(defn- spit-deps-total []
  (spit "deps-total.json"
        (json/write-value-as-string
         {:deps-total (count @deps)}))
  (println "Added deps-total.json"))

(defn- spit-deps-top []
  (spit "deps-top.json"
        (json/write-value-as-string
         (->> @deps
              (sort-by #(count (:repos %)))
              reverse
              (take 100))))
  (println "Added deps-top.json"))

(defn -main []
  ;; Read reuse.json, update old reuse information, merge the result
  ;; and spit it to reuse.json.
  (spit-reuse-info)
  ;; Update @repos by adding dependencies or updating old ones.
  (update-repos-deps)
  ;; Update @deps with valid dependencies.
  (validate-repos-deps)
  ;; Update @repos with valid dependencies and spit repos-deps.json.
  (spit-repos-deps)
  ;; Update @deps by adding :repos and spit deps.json.
  (spit-deps-with-repos)
  ;; ;; Spit deps-repos.json
  (spit-deps-repos)
  ;; ;; Spit deps-orgas.json
  (spit-deps-orgas)
  ;; ;; Spit deps-total.json
  (spit-deps-total)
  ;; ;; Spit deps-top.json
  (spit-deps-top))

;; (-main)
