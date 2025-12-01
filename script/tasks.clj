(ns tasks
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- parse-args [opts]
  (let [year (get opts :y 2025)
        day (get opts :d 1)]
    {:year (if (string? year) (parse-long year) year)
     :day  (if (string? day) (parse-long day) day)}))

(defn- read-session-token []
  (let [env-file "env.edn"]
    (if (fs/exists? env-file)
      (:session-token (edn/read-string (slurp env-file)))
      (throw (ex-info "env.edn not found! Create it with {:session-token \"your-token\"}" {})))))

(defn download [opts]
  (let [{:keys [year day]} (parse-args opts)
        filename (str "data/d" day ".txt")
        url (format "https://adventofcode.com/%s/day/%s/input" year day)
        session-token (read-session-token)]
    
    (when (fs/exists? filename)
      (println (format "⚠️  Input already exists: %s" filename))
      (System/exit 0))
    
    (println (format "📥 Downloading input for %s day %s..." year day))
    
    (let [response (http/get url {:headers {"Cookie" (str "session=" session-token)}})]
      (if (= 200 (:status response))
        (do
          (spit filename (:body response))
          (println (format "✅ Saved to %s" filename)))
        (println (format "❌ Failed to download (status %s)" (:status response)))))))


(defn template [opts]
  (let [{:keys [year day]} (parse-args opts)
        dir (str "src/y" year)
        filename (str dir "/d" day ".clj")
        input-path (format "data/y%s/d%s.txt" year day)]
    
    (when (fs/exists? filename)
      (println (format "⚠️  Template already exists: %s" filename))
      (System/exit 0))
    
    (println (format "📝 Creating template for %s day %s..." year day))
    
    (let [template-str (format "(ns y%s.d%s
  (:require [clojure.string :as str]))

;; --- Input parsing ---

(def input (slurp \"%s\"))

(defn parse [s]
  (->> (str/split-lines s)
       ;; parse here
       ))

(def data (parse input))
" year day input-path)]
      
      (fs/create-dirs dir)
      (spit filename template-str)
      (println (format "✅ Created %s" filename)))))

;; --- Setup (download + template) ---

(defn setup [opts]
  (download opts)
  (template opts)
  (println "\n🎄 Ready to solve! Start REPL with: bb repl"))