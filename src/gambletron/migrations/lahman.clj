(ns gambletron.migrations.lahman
  "Initial population of the database with Lahman's data"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure-csv.core :as csv]
            [clj-time.format :as f]
            [korma.db :as db]
            (gambletron [util :as util]
                        [schema :as schema]
                        [team :as team]
                        [batting :as batting]
                        [pitching :as pitching]
                        [fielding :as fielding]
                        [player :as player])))

;;; utility functions to parse the data
(defn- string-resembles-date? [s]
  (when (string? s)
    (or (= s (re-find #"\d{4}-\d{1,2}-\d{1,2}" s))
        (= s (re-find #"\d{1,2}/\d{1,2}/\d{4}" s)))))

(defn- string-resembles-int? [s]
  (and (string? s)
       (or (string/blank? s)
           (= s (re-find #"\d+" s)))))

(defn- string-resembles-float? [s]
  (and (string? s)
       (= s (re-find #"\d+\.\d+" s))))

(defn parse-int [x & [blank-is-zero?]]
  (when (string-resembles-int? x)
    (if (string/blank? x)
      (when blank-is-zero?
        0) ; otherwise HBP, SF, and others would be "" instead of 0
      (Long/parseLong x))))

(defn parse-double [x]
  (when (string-resembles-float? x)
    (Double/parseDouble x)))

(defn parse-date
  "Produces an RFC3339 string YYYY-MM-DD. See http://tools.ietf.org/html/rfc3339"
  [x]
  (when (string-resembles-date? x)
    (let [date-pattern (if (= x (re-find #"\d{4}-\d{1,2}-\d{1,2}" x)) #"-" #"/")
          [month day year] (string/split x date-pattern)]
      (format "%d-%02d-%02d"
              (parse-int year)
              (parse-int month)
              (parse-int day)))))

(def ^:dynamic *blank-is-zero?* false)

(defn- parse-entry [x]
  (or (parse-date x)
      (parse-double x)
      (parse-int x *blank-is-zero?*)
      (when-not (string/blank? x)
        x)))


(defn- directory? [f]
  (.isDirectory f))

(def resources-dir (io/file (str (System/getProperty "user.dir")
                                 "/resources/")))

(defn- lahman-dir-date [dir]
  (f/unparse (f/formatters :year-month-day)
             (re-find #"\d{4}-\d{2}-\d{2}" (.getName dir))))

(defn- path-to-latest-lahman-data []
  (->> resources-dir
       file-seq
       (filter directory?)
       (filter (fn [dir]
                 (.startsWith (.getName dir) "lahman-")))
       (sort-by lahman-dir-date)
       last))

(defn- load-data* [path]
  (with-open [file (io/reader path)]
    (let [[column-keys & rows] (csv/parse-csv file)
          ks (map keyword column-keys)]
      (reduce (fn [data-seq row]
                (conj data-seq
                      (util/dissoc-nils (zipmap ks
                                                (map parse-entry row)))))
              []
              rows))))

(defn load-data [filename]
  (load-data* (str (path-to-latest-lahman-data) "/" filename)))

;;; utility functions to transform the data to populate the database
(defn- populate-table [file-name create-fn transform-fn]
  (let [data (map transform-fn (load-data file-name))
        pph (quot (count data) 100)]
  (doseq [[i row] (map list (rest (range)) data)]
    (when (zero? (mod i (* 5 pph)))
      (println (quot i pph) "%"))
    (create-fn row))))

(defn- transform-id-keys [data-map]
  (util/rename-keys data-map
                    {:playerID :player-id
                     :yearID :year-id
                     :teamID :team-id
                     :lgID :league-id}))

(defn populate-batting []
  (populate-table "Batting.csv" batting/create (comp schema/transform-batting transform-id-keys)))

(defn populate-pitching []
  (populate-table "Pitching.csv" pitching/create transform-id-keys))

(defn- transform-player-keys [player-map]
  (util/rename-keys player-map
                    {:retroID :retro-id
                     :bbrefID :bbref-id
                     :playerID :id}))

(defn populate-player []
  (populate-table "Master.csv" player/create (comp schema/transform-player transform-player-keys)))

(defn- transform-team-keys [team-map]
  (util/rename-keys team-map
                    {:yearID :year-id
                     :lgID :league-id
                     :teamID :lahman-id
                     :franchID :franchise-id
                     :divID :division-id
                     :G :games-played
                     :Ghome :home-games
                     :W :wins
                     :L :losses
                     :DivWin :division-winner
                     :WCWin :wild-card-winner
                     :LgWin :league-champion
                     :WSWin :world-series-winner
                     :R :runs
                     :AB :at-bats
                     :H :hits
                     :2B :doubles
                     :3B :triples
                     :HR :homeruns
                     :park :park-name
                     :teamIDBR :baseball-reference-id
                     :teamIDlahman45 :old-lahman-id
                     :teamIDretro :retrosheet-id}))

(defn- yes-no->bool [map k]
  (assoc map k (if (= "Y" (k map))
                 1
                 (when (= "N" (k map))
                   0))))

(defn- transform-team [team-map]
  (doall
   (-> team-map
       transform-team-keys
       (yes-no->bool :division-winner)
       (yes-no->bool :wild-card-winner)
       (yes-no->bool :league-champion)
       (yes-no->bool :world-series-winner))))

(defn populate-team []
  (populate-table "Teams.csv" team/create transform-team))

(defn populate-fielding []
  (populate-table "Fielding.csv" fielding/create transform-id-keys))

;;; big red button, just hit this to load the data into the SQL database
(defn migrate []
  (db/transaction
   (println "Loading teams")
   (populate-team))
  (db/transaction
   (println "Loading players")
   (populate-player))
  (db/transaction
   (println "Loading fielding stats")
   (populate-fielding))
  (binding [*blank-is-zero?* true]
    (db/transaction
     (println "Loading pitching stats")
     (populate-pitching))
    (db/transaction
     (println "Loading batting stats")
     (populate-batting)))
  :done)

