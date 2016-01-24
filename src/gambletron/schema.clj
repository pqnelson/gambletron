(ns gambletron.schema
  (:require (clj-time [coerce :as coerce]
                      [core :as time]
                      [format :as format])
            [clojure.string :as string]
            [korma.core :refer :all :as korma]
            [korma.db :refer :all]
            [gambletron.util :refer [defconst] :as util]))

(def db-dir (str (System/getProperty "user.dir") "/resources/"))

(defn- underscores->dashes [n]
  (-> n util/snake->kabob keyword))

(defn- dashes->underscores [n]
  (-> n name util/kabob->snake))

(def dash-naming-strategy {:keys underscores->dashes
                           :fields dashes->underscores})

(defdb db (postgres {:db "gambletron"
                     :user "gambletron"
                     :password "gambletron"
                     :naming dash-naming-strategy}))

(def chadwick-format (format/formatter "yyyyMMdd"))

(defn- chadwick-date->clj-date [datetime-int]
  (format/parse chadwick-format (str datetime-int)))

(defn- clj-date->chadwick-date [datetime]
  (when datetime
    (format/unparse chadwick-format datetime)))

(def iso-format (format/formatters :date))
(defn- to-sql-date [datetime]
  (when datetime
    (coerce/to-sql-date datetime)))

(defn- to-clj-date [sql-date]
  (when sql-date
    (coerce/from-sql-date sql-date)))

(declare batting pitching fielding player teams events)

(defrecord Player [id
                   birthday
                   birthCountry
                   birthState
                   birthCity
                   died
                   deathCountry
                   deathState
                   deathCity
                   first-name
                   last-name
                   given-name
                   weight
                   height
                   bats
                   throws
                   debut
                   finalGame
                   lahman-id
                   bbref-id])

(defn transform-player [player-map]
  (-> player-map 
      (assoc :died (to-clj-date (:died player-map)))
      (assoc :birthday (to-clj-date (:birthday player-map)))
      (assoc :debut (to-clj-date (:debut player-map)))
      (assoc :finalGame (to-clj-date (:finalGame player-map)))
      util/dash-keys
      map->Player))

(defn prepare-player [v]
  (-> v
      (assoc :died (to-sql-date (:died v)))
      (assoc :birthday (to-sql-date (:birthday v)))
      (assoc :debut (to-sql-date (:debut v)))
      (assoc :final-game (to-sql-date (:final-game v)))
      util/underscore-keys))

(defentity player
  (entity-fields :id :birthday :birth_country :birth_state :birth_city
                 :died :death_country :death_state :death_city 
                 :first_name :last_name :given_name :weight :height :bats
                 :throws :debut :final_game :lahman_id :bbref_id) 
  (pk :id)
  (prepare prepare-player)
  (transform transform-player))

(defrecord Batting [id
                    player-id
                    year-id
                    stint
                    team-id
                    league-id
                    G      ; games
                    AB     ; at bats
                    R      ; runs
                    H      ; hits
                    HR     ; homeruns
                    RBI    ; runs batted in
                    SB     ; stolen bases
                    CS     ; caught stealing
                    BB     ; base on balls
                    SO     ; strikeouts
                    IBB    ; intentional walks
                    HBP    ; hit by pitch
                    SH     ; sacrifice hits
                    SF     ; sacrifice flies
                    GIDP]) ; grounded into double plays

(defn prepare-batting [v]
  (-> v
      util/underscore-keys))

(defn transform-batting [v]
  (-> v
      util/dash-keys
      map->Batting))

(defentity batting
  (entity-fields :id :player_id :year_id :stint :team_id :league_id :G
  :AB :R :H :2B :3B :HR :RBI :SB :CS :BB :SO :IBB :HBP :SH :SF :GIDP) 
  (pk :id)
  (prepare prepare-batting)
  (transform transform-batting)
  (belongs-to player))

(defrecord Pitching [id
                     player-id
                     year-id
                     stint
                     team-id
                     league-id
                     W      ; wins
                     L      ; losses
                     G      ; games
                     GS     ; games started
                     CG     ; complete games 
                     SHO    ; shutouts 
                     SV     ; saves
                     IPOuts ; Outs pitched (innings pitched * 3)
                     H      ; hits
                     ER     ; earned runs
                     HR     ; homeruns
                     BB     ; walks
                     SO     ; strikeouts
                     BAOpp  ; opponent's batting average
                     ERA    ; earned run average
                     IBB    ; intentional walks
                     WP     ; wild pitches
                     HBP    ; batters hit by pitch
                     BK     ; balks
                     BFP    ; batters faced by pitcher
                     GF     ; games finished
                     R      ; runs allowed
                     SH     ; sacrifices by opposing batters
                     SF     ; sacrifice flies by opposing batters
                     GIDP]) ; grounded into double plays by opposing batter

(defentity pitching
  (entity-fields :id :player_id :year_id :stint :team_id :league_id :W
  :L :G :GS :CG :SHO :SV :IPOuts :H :ER :HR :BB :SO :BAOpp :ERA :IBB :WP
  :HBP :BK :BFP :GF :R :SH :SF :GIDP) 
  (pk :id)
  (prepare (fn [v]
             (-> v
                 util/underscore-keys)))
  (transform (fn [v]
               (-> v
                   util/dash-keys
                   map->Pitching)))
  (belongs-to player))

;; (defrecord Fielding [id
;;                      player-id
;;                      year-id
;;                      stint
;;                      team-id
;;                      league-id
;;                      Pos ; position
;;                      G ; games
;;                      GS ; games started
;;                      InnOuts ; Time played in the field expressed as outs
;;                      PO ; putouts
;;                      A ; assists
;;                      E ; errors 
;;                      DP ; double plays
;;                      PB ; passed balls (by catchers)
;;                      WP ; wild pitches (by catchers)
;;                      SB ; opponent stolen bases (by catchers)
;;                      CS ; opponents caught stealing (by catchers)
;;                      ZR]) ; zone rating

;; (defentity fielding
;;   (entity-fields :id :player_id :year_id :stint :team_id :league_id :Pos :G :GS :InnOuts :PO :A :E :DP :PB :WP :SB :CS :ZR)
;;   (pk :id)
;;   (prepare (fn [v]
;;              (-> v
;;                  util/underscore-keys)))
;;   (transform (fn [v]
;;                (-> v
;;                    util/dash-keys
;;                    map->Fielding)))
;;   (belongs-to player))



(defrecord Team [team-id
                 lg-id
                 loc-team-tx
                 name-team-tx])

(defentity teams
  (pk :id)
  (prepare (fn [v]
             (-> v
                 util/underscore-keys)))
  (transform (fn [v]
               (-> v
                   util/dash-keys
                   map->Team)))
  (has-many player))

(defrecord Game [game-id
                 away-team-id
                 home-team-id
                 away-score             ; final score for visitors
                 home-score             ; final score for home team
                 game-ct                ; count
                 started                ; datetime game started
                 ended                  ; datetime game ended
                 attendance
                 park-id
                 innings                ; number of innings
                 sky-park-cd            ; sky conditions
                 field-park-cd          ; field conditions
                 temp-park-ct           ; temperature
                 wind-direction-park-cd ; wind direction
                 wind-speed-park-cd     ; wind speed
                 precip-park-cd         ; precipitation
                 ])

;; (defn scatter [gs]
;;   (charts/scatter-plot (map :temp-park-ct gs) (map :home-score-ct gs)))

(defconst game-park-field {0 :unknown
                           1 :soaked
                           2 :wet
                           3 :damp
                           4 :dry})
(defconst game-precipitation {0 :unknown
                              1 :none
                              2 :drizzle
                              3 :showers
                              4 :rain
                              5 :snow})
(defconst game-sky {0 :unknown
                    1 :sunny
                    2 :cloudy
                    3 :overcast
                    4 :night
                    5 :dome})
(defconst game-wind-dir {0 :unknown
                         1 :to-left-field
                         2 :to-center-field
                         3 :to-right-field
                         4 :left-field->right-field
                         5 :from-left-field
                         6 :from-center-field
                         7 :from-right-field
                         8 :right-field->left-field})



(defn- parse-started [v]
  (when-let [game-date (format/parse (format/formatters :basic-date)
                                     (subs (:game-id v) 3 11))]
    (time/plus game-date
               (time/hours (+ (if (= "N" (:daynight-park-cd v))
                                12
                                0)
                              (quot (:start-game-tm v) 100)))
               (time/minutes (mod (:start-game-tm v) 100)))))

(defn- parse-ended [v]
  (when-let [started (parse-started v)]
    (time/plus started (time/minutes (:minutes-game-ct v)))))

(defn transform-game [v]
  (-> v
      util/dash-keys
      (assoc :started (parse-started v))
      (assoc :ended (parse-ended v))
      ;; (dissoc :game-date :year :start-game-tm :minutes-game-ct)
      (util/rename-keys {:inn-ct :innings
                         :attend-park-ct :attendance
                         :daynight-park-cd :day-or-night})))

(defentity games
  (pk :game-id)
  (prepare (fn [v]
             (Error. "Trying to save a game, not allowed!")))
  (transform transform-game)
  (has-many teams)
  (has-many events)
  )

(defentity vw_games)

(defentity rosters
  )

(defn- read-int [m field]
  (try
    (assoc m field (read-string (field m)))
    (catch Exception e m)))

(defn- transform-event [event-map]
  (-> event-map
      (read-int :event-cd)
      (read-int :event-id)))

(defentity events
  (transform transform-event))

(defn to-clj-time [m fields]
  (into m
        (for [[k v] (select-keys m fields)
              :when v]
          [k (coerce/from-sql-time v)])))

(defentity vw_events
  (transform (fn [v]
               (-> v
                   transform-event
                   (to-clj-time [:game-date])))))

;; Additional commands you might want to run:
;;
;; CREATE INDEX game_away_team_id_idx ON games (away_team_id,game_dt);
;; CREATE INDEX game_home_team_id_idx ON games (home_team_id,game_dt);
;; CREATE INDEX event_batter_id_idx ON events (bat_id);
;; CREATE INDEX event_pitcher_id_idx ON events (pit_id);
;; ALTER TABLE games ADD FOREIGN KEY (home_team_id) REFERENCES teams;
;; ALTER TABLE games ADD FOREIGN KEY (away_team_id) REFERENCES teams;
;; ALTER TABLE games ADD FOREIGN KEY (park_id) REFERENCES parkcodes;
;; ALTER TABLE events ADD FOREIGN KEY (game_id) REFERENCES games;
;; ALTER TABLE events ADD FOREIGN KEY (home_team_id) REFERENCES teams;
;; ALTER TABLE events ADD FOREIGN KEY (away_team_id) REFERENCES teams;
;; ALTER TABLE events ADD FOREIGN KEY (substring(game_id, 4, 4)::integer,bat_id,,bat_team_id) REFERENCES rosters(year,player_id,team_tx);
;; ALTER TABLE events ADD FOREIGN KEY (substring(game_id, 4, 4)::integer,pit_id,fld_team_id) REFERENCES rosters;
