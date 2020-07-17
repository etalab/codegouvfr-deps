;; Copyright (c) 2020 DINUM, Bastien Guerry <bastien.guerry@data.gouv.fr>
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSE

(ns core
  (:require  [cheshire.core :as json]
             [babashka.curl :as curl]
             [clojure.data.xml :as xml]
             [clojure.string :as s]
             [clojure.edn :as edn]
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

(defonce repos
  (atom
   (when-let [res (try (curl/get repos-url)
                       (catch Exception e
                         (println "ERROR: Cannot reach repos-url\n"
                                  (.getMessage e))))]
     (json/parse-string (:body res) true))))

(when-let [deps (not-empty (filter not-empty (map :deps @repos)))]
  (def deps
    (atom
     (reduce #(merge-with into %1 %2) deps))))

;; Utility functions

(defn- less-than-a-week-ago [date-str]
  (let [one-week-ago (t/minus (t/instant) (t/days 7))]
    (= (t/min (t/instant date-str) one-week-ago)
       one-week-ago)))

;; Module validation

(defn- get-valid-npm [module]
  (let [registry-url-fmt "https://registry.npmjs.org/-/v1/search?text=%s&size=1"]
    (when-let [res (try (curl/get (format registry-url-fmt module))
                        (catch Exception _ nil))]
      (let [{:keys [description links]}
            (-> (:body res)
                (json/parse-string true)
                :objects first :package)]
        {:name module :description description :link (:npm links)}))))

(defn- get-valid-pypi [module]
  (let [registry-url-fmt "https://pypi.org/pypi/%s/json"]
    (when-let [res (try (curl/get (format registry-url-fmt module))
                        (catch Exception _ nil))]
      (let [{:keys [info]}
            (-> (:body res)
                (json/parse-string true))]
        {:name        module
         :description (:summary info)
         :link        (:package_url info)}))))

;; FIXME: Where to get a proper maven artifact description?
(defn- get-valid-maven [module]
  (let [[groupId artifactId] (drop 1 (re-find #"([^/]+)/([^/]+)" module))
        registry-url-fmt
        "https://search.maven.org/solrsearch/select?q=g:%%22%s%%22+AND+a:%%22%s%%22&core=gav&rows=1&wt=json"
        link-fmt
        "https://search.maven.org/classic/#search|ga|1|g:%%22%s%%22%%20AND%%20a:%%22%s%%22"]
    (when-let [res (try (curl/get (format registry-url-fmt groupId artifactId))
                        (catch Exception _ nil))]
      (let [tags (-> (json/parse-string (:body res) true)
                     :response
                     :docs
                     first
                     :tags)]
        {:name        module
         :description (s/join ", " (take 6 tags))
         :link        (format link-fmt groupId artifactId)}))))

(defn- get-valid-clojars [module]
  (let [registry-url-fmt "https://clojars.org/api/artifacts/%s"]
    (when-let [res (try (curl/get (format registry-url-fmt module))
                        (catch Exception _ nil))]
      {:name        module
       :description (:description (json/parse-string (:body res) true))
       :link        (str "https://clojars.org/" module)})))

(defn- get-valid-bundler [module]
  (let [registry-url-fmt "https://rubygems.org/api/v1/gems/%s.json"]
    (when-let [res (try (curl/get (format registry-url-fmt module))
                        (catch Exception _ nil))]
      (let [{:keys [info project_uri]} (json/parse-string (:body res) true)]
        {:name        module
         :description info
         :link        project_uri}))))

(defn- get-valid-composer [module]
  (let [registry-url-fmt "https://packagist.org/packages/%s"]
    (when-let [res (try (curl/get (str (format registry-url-fmt module) ".json"))
                        (catch Exception _ nil))]
      {:name        module
       :description (:description (:package (json/parse-string (:body res) true)))
       :link        (format registry-url-fmt module)})))

;; Reuse information

(defn add-reuse
  "Return a hash-map with the repo and the number of reuse."
  [{:keys [repertoire_url plateforme reuse_updated] :as repo}]
  (if (or (not (= "GitHub" plateforme))
          (not (re-find #"^https?://github\.com" repertoire_url))
          (when-let [d (not-empty reuse_updated)]
            (less-than-a-week-ago d)))
    repo
    ;; Only check available information on github.com
    (when-let [repo-github-html
               (try (curl/get (str repertoire_url "/network/dependents"))
                    (catch Exception e
                      (println "Cannot get"
                               (str repertoire_url "/network/dependents\n")
                               (.getMessage e))))]
      (let [btn-links (-> repo-github-html
                          :body
                          h/parse
                          h/as-hickory
                          (as-> d (hs/select (hs/class "btn-link") d)))
            nb-reps   (or (try (re-find #"\d+" (last (:content (nth btn-links 1))))
                               (catch Exception _ "0"))
                          0)
            nb-pkgs   (or (try (re-find #"\d+" (last (:content (nth btn-links 2))))
                               (catch Exception _ "0"))
                          0)]
        (assoc
         repo
         :reuse_updated (str (t/instant))
         :reuse         (+ (Integer/parseInt nb-reps) (Integer/parseInt nb-pkgs)))))))

(defn add-reuse-info
  "Update @repos with GitHub reused-by information."
  []
  (reset! repos (doall (map add-reuse @repos)))
  (println "Added reuse information"))

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
  (when-let [deps0 (filter #(= (name (:tag %)) "dependencies")
                           (->> (:content (xml/parse-str body))
                                (remove string?)))]
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
            (less-than-a-week-ago d)))
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

(defn- add-repos-deps
  "Update @repos with dependencies information."
  []
  (let [res (atom [])]
    (doseq [r @repos]
      (let [deps (add-dependencies r)]
        (swap! res conj deps)))
    (reset! repos @res))
  (println "Added dependencies information"))

(defn -main []
  (add-repos-deps)
  (add-reuse-info)
  (spit "deps.json" (json/generate-string @repos)))
