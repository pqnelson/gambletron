(ns gambletron.migrations.lahman
  "Initial population of the database with Lahman's data"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure-csv.core :as csv]
            [clj-time.format :as f]
            [clj-time.core :as time]
            [korma.core :as korma]
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
        (= s (re-find #"\d{2}-\d{4}-\d{2}" s))
        (= s (re-find #"\d{1,2}/\d{1,2}/\d{4}" s)))))

(def multi-parser (f/formatter (time/default-time-zone)
                               "dd-YYYY-MM"
                               "MM/dd/YYYY"
                               "YYYY-MM-dd"))

(defn- parse-date [final-game]
  (when (string-resembles-date? final-game)
    (f/parse multi-parser final-game)))

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

(def lahman-lookup (atom nil))

(defn- reset-lahman-lookup! []
  (let [results (korma/exec-raw ["SELECT id,lahman_id FROM player"
                                 []] :results)]
    (reset! lahman-lookup
            (into {}
                  (for [row results]
                    [(:lahman-id row) (:id row)])))))

(defn translate-player-id* [player-id]
  {:post [(or (not *player-id-needed?*)
              (not (string/blank? %)))]}
  ((or @lahman-lookup
       (do (reset-lahman-lookup!)
           @lahman-lookup))
       player-id))

(def ^:dynamic *player-id-needed?* false)

(defn translate-player-id [{:keys [player-id] :as m}]
  (if player-id
    (assoc m :player-id (translate-player-id* (:player-id m)))
    (when-not *player-id-needed?*
      m)))

(def ^:dynamic *team-id-needed?* false)

(def team-ids (set (map :team-id (korma/exec-raw ["SELECT team_id FROM teams" []] :results))))

(defn- translate-lahman-team-id [{:keys [team-id] :as m}]
  (if (#{"LAA"} team-id)
    (assoc m :team-id "ANA")
    (if (team-ids team-id)
      m
      (when (> (:year-id m) 2000) (println "CANNOT FIND:" team-id)))))

(defn filter-team-id [maps]
  (let [ids (set (map :team-id (korma/exec-raw ["SELECT team_id FROM teams" []] :results)))]
    (if-not *team-id-needed?*
      maps
      (filter (comp ids :team-id)
              (map translate-lahman-team-id maps)))))

;;; utility functions to transform the data to populate the database
(defn- populate-table
  [file-name create-fn transform-fn & [{:keys [check-fn] :or {check-fn
                                                             (constantly
                                                              true)}}]]
  (let [data (filter-team-id (map transform-fn (load-data file-name)))
        pph (quot (count data) 100)]
    (doseq [[i row] (map list (rest (range)) data)]
      (when (#{"troutmi01" "troum001"} (:player-id row))
        (println row)
        (when-not (check-fn row)
          (println "Will not be added to the database...")))
      (when (check-fn row)
        (when (zero? (mod i (* 5 pph)))
          (println (quot i pph) "%"))
        (create-fn row)))))

(defn- transform-id-keys [data-map]
  (util/rename-keys data-map
                    {:IPouts :IPOuts
                     :playerID :player-id
                     :yearID :year-id
                     :teamID :team-id
                     :lgID :league-id}))

(defn make-batting-table []
  (try
  (korma/exec-raw [(str "CREATE TABLE batting("
                        "       id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),"
                        "       player_id TEXT NOT NULL REFERENCES player(id),"
                        "       year_id INTEGER NOT NULL,"
                        "       stint INTEGER NOT NULL,"
                        "       team_id TEXT NOT NULL REFERENCES teams(team_id),"
                        "       league_id TEXT NOT NULL,"
                        "       \"G\" INT DEFAULT 0,"
                        "       \"AB\" INT DEFAULT 0,"
                        "       \"R\" INT DEFAULT 0,"
                        "       \"H\" INT DEFAULT 0,"
                        "       \"2B\" INT DEFAULT 0,"
                        "       \"3B\" INT DEFAULT 0,"
                        "       \"HR\" INT DEFAULT 0,"
                        "       \"RBI\" INT DEFAULT 0,"
                        "       \"SB\" INT DEFAULT 0,"
                        "       \"CS\" INT DEFAULT 0,"
                        "       \"BB\" INT DEFAULT 0,"
                        "       \"SO\" INT DEFAULT 0,"
                        "       \"IBB\" INT DEFAULT 0,"
                        "       \"HBP\" INT DEFAULT 0,"
                        "       \"SH\" INT DEFAULT 0,"
                        "       \"SF\" INT DEFAULT 0,"
                        "       \"GIDP\" INT DEFAULT 0"
                        "       );")
                   []])
  (catch Exception e
    (println (.getNextException e)))))

(defn- check-batting [batting-row]
  (empty? (korma/select
           schema/batting
           (korma/where {:year-id (:year-id batting-row)
                         :stint (:stint batting-row)
                         :player-id (:player-id batting-row)
                         :team-id (:team-id batting-row)}))))

(defn populate-batting []
  (binding [*blank-is-zero?* true
            *player-id-needed?* true
            *team-id-needed?* true]
    (populate-table "Batting.csv"
                    batting/create
                    (comp translate-player-id util/dissoc-nils transform-id-keys)
                    {:check-fn check-batting})))

(defn make-pitching-table []
  (try
  (korma/exec-raw [(str
                    "CREATE TABLE pitching("
                    "       id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),"
                    "       player_id TEXT NOT NULL  REFERENCES player(id),"
                    "       year_id INTEGER NOT NULL,"
                    "       stint INTEGER NOT NULL,"
                    "       team_id TEXT NOT NULL  REFERENCES teams(team_id),"
                    "       league_id TEXT NOT NULL,"
                    "       \"W\" INT DEFAULT 0,"
                    "       \"L\" INT DEFAULT 0,"
                    "       \"G\" INT DEFAULT 0,"
                    "       \"GS\" INT DEFAULT 0,"
                    "       \"CG\" INT DEFAULT 0,"
                    "       \"SHO\" INT DEFAULT 0,"
                    "       \"SV\" INT DEFAULT 0,"
                    "       \"IPOuts\" INT DEFAULT 0,"
                    "       \"H\" INT DEFAULT 0,"
                    "       \"ER\" INT DEFAULT 0,"
                    "       \"HR\" INT DEFAULT 0,"
                    "       \"BB\" INT DEFAULT 0,"
                    "       \"SO\" INT DEFAULT 0,"
                    "       \"BAOpp\" REAL,"
                    "       \"ERA\" REAL,"
                    "       \"IBB\" INT DEFAULT 0,"
                    "       \"WP\" INT DEFAULT 0,"
                    "       \"HBP\" INT DEFAULT 0,"
                    "       \"BK\" INT DEFAULT 0,"
                    "       \"BFP\" INT DEFAULT 0,"
                    "       \"GF\" INT DEFAULT 0,"
                    "       \"R\" INT DEFAULT 0,"
                    "       \"SH\" INT DEFAULT 0,"
                    "       \"SF\" INT DEFAULT 0,"
                    "       \"GIDP\" INT DEFAULT 0"
                    "       );")
                   []])
  (catch Exception e
    (println (.getNextException e)))))

(defn- check-pitching [pitching-row]
  (empty? (korma/select
           schema/pitching
           (korma/where {:year-id (:year-id pitching-row)
                         :stint (:stint pitching-row)
                         :player-id (:player-id pitching-row)
                         :team-id (:team-id pitching-row)}))))

(defn populate-pitching []
  (binding [*blank-is-zero?* true
            *player-id-needed?* true
            *team-id-needed?* true]
    (populate-table "Pitching.csv"
                    pitching/create
                    (comp translate-player-id
                          util/dissoc-nils
                          transform-id-keys)
                    {:check-fn check-pitching})))

(defn- birthday [{:keys [birthYear birthMonth birthDay] :as player-map}]
  (if (and birthYear birthMonth birthDay)
    (time/date-time birthYear birthMonth birthDay)
    (println "Passed" player-map)))

(defn- died [{:keys [deathYear deathMonth deathDay]}]
  (when (and deathYear deathMonth deathDay)
    (time/date-time deathYear deathMonth deathDay)))

(defn- assoc-player-id [player-map]
  (if (:id player-map)
    player-map
    (assoc player-map :id (:lahman-id player-map))))

(defn- transform-player-keys [player-map]
  (-> player-map
      (util/rename-keys {:retroID :id
                         :birthCity :birth-city
                         :birthCountry :birth-country
                         :birthState :birth-state
                         :deathCity :death-city
                         :deathCountry :death-country
                         :deathState :death-state
                         :bbrefID :bbref-id
                         :playerID :lahman-id
                         :nameLast :last-name
                         :nameFirst :first-name
                         :nameGiven :given-name})
      assoc-player-id
      (assoc :birthday (birthday player-map))
      (assoc :final-game (when (:finalGame player-map)
                           (parse-date (:finalGame player-map))))
      (assoc :debut (when (:debut player-map)
                      (parse-date (:debut player-map))))
      (assoc :died (died player-map))
      (dissoc :birthYear :birthMonth :birthDay :finalGame
              :deathYear :deathMonth :deathDay)))

(defn populate-player []
  (populate-table "Master.csv" (comp player/create schema/prepare-player) (comp util/dash-keys transform-player-keys)))

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

