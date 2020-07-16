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
             [hickory.select :as hs])
  (:gen-class))

;; Setup definitions

(defonce dep-files
  {"PHP"        ["composer.json"]
   "Vue"        ["package.json"]
   "JavaScript" ["package.json"]
   "TypeScript" ["package.json"]
   "Python"     ["setup.py"]
   "Ruby"       ["Gemfile"]
   "Java"       ["pom.xml"]
   "Clojure"    ["pom.xml" "deps.edn" "project.clj"]})

(defonce repos-url
  "https://raw.githubusercontent.com/etalab/data-codes-sources-fr/master/data/repertoires/json/all.json")

;; Core functions

(defonce repos
  (atom
   (when-let [res (try (curl/get repos-url)
                       (catch Exception e
                         (println "ERROR: Cannot reach repos-url\n"
                                  (.getMessage e))))]
     (json/parse-string (:body res) true))))

(defn get-reused-by
  "Return a hash-map with the repo and the number of reuse."
  [repo]
  ;; Only check available information on github.com
  (if-not (re-find #"github\.com" repo)
    {:repertoire_url repo :reused "N/A"}
    (when-let [repo-github-html
               (try (curl/get (str repo "/network/dependents"))
                    (catch Exception e
                      (println "Cannot get"
                               (str repo "/network/dependents\n")
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
        {:repertoire_url repo
         :reused         (+ (Integer/parseInt nb-reps) (Integer/parseInt nb-pkgs))}))))

(defn reuse
  "Update repos.json with reused-by info."
  []
  (spit "reuse.json"
        (json/generate-string
         (->> @repos
              (map :repertoire_url)
              (filter #(not (re-find #"^http://github\.com" %)))
              (map get-reused-by))))
  (println "Added reuse.son"))

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
  (let [deps0 (filter #(= (name (:tag %)) "dependencies")
                      (->> (:content (xml/parse-str body))
                           (remove string?)))
        deps  (remove #(re-find #"^org\.clojure" %)
                      (flatten
                       (map #(:content (second (:content %)))
                            (remove string? (:content (first deps0))))))]
    (when (seq deps)
      {:maven (into [] deps)})))

(defn- repo-check-dep-files
  "Take a repository map and return the map completed with dependencies."
  [{:keys
    [repertoire_url organisation_nom
     nom plateforme langage] :as repo}]
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
                       "Gemfile"
                       (get-gemfile-deps body)
                       "deps.edn"
                       (get-depsedn-deps body)
                       "project.clj"
                       (get-projectclj-deps body)
                       "pom.xml"
                       (get-pomxml-deps body))]
            (swap! deps #(merge-with into % reqs))))))
    (assoc repo :deps @deps)))

(defn- repos-deps []
  (let [res (atom [])]
    (doseq [r (->> @repos
                   (filter #(not (= (:langage %) "")))
                   (filter #(not (= (:est_archive %) true))))]
      (let [deps (repo-check-dep-files r)]
        (when (seq deps)
          (swap! res conj deps))))
    (spit "deps.json" (json/generate-string @res))))

(defn -main []
  (repos-deps)
  (reuse))
