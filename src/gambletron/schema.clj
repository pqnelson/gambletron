(ns gambletron.schema
  (:require [clj-time.core :as time]
            [clj-time.format :as format]
            [clojure.string :as string]
            [korma.core :refer :all :as korma]
            [korma.db :refer :all]
            [gambletron.util :as util]))


(def db-dir (str (System/getProperty "user.dir") "/resources/"))

(defn- underscores->dashes [n]
  (-> n util/snake->kabob keyword))

(defn- dashes->underscores [n]
  (-> n name util/kabob->snake))

(def dash-naming-strategy {:keys underscores->dashes
                           :fields dashes->underscores})

(defdb db (sqlite3 {:db "resources/baseball.db"
                    :naming dash-naming-strategy}))

(def iso-format (format/formatters :date))
(defn- to-sql-date [datetime]
  (when datetime
    (format/unparse iso-format datetime)))

(defn- to-clj-date [datetime-string]
  (when-not (string/blank? datetime-string)
    (format/parse datetime-string)))

(declare batting pitching fielding player team)

(defrecord Player [id
                   birthYear
                   birthMonth
                   birthDay
                   birthCountry
                   birthState
                   birthCity
                   deathYear
                   deathMonth
                   deathDay
                   deathCountry
                   deathState
                   deathCity
                   nameFirst
                   nameLast
                   nameGiven
                   weight
                   height
                   bats
                   throws
                   debut
                   finalGame
                   retro-id
                   bbref-id])

(defn transform-player [player-map]
  (-> player-map 
      (assoc :debut (to-clj-date (:debut player-map)))
      (assoc :finalGame (to-clj-date (:finalGame player-map)))
      util/dash-keys
      map->Player))

(defentity player
  (entity-fields :id :birthYear :birthMonth :birthDay :birthCountry
                 :birthState :birthCity :deathYear :deathMonth
                 :deathDay :deathCountry :deathState :deathCity
                 :nameFirst :nameLast :nameGiven :weight :height :bats
                 :throws :debut :finalGame :retro_id :bbref_id) 
  (pk :id)
  (prepare (fn [v]
               (-> v
                   (assoc :debut (to-sql-date (:debut v)))
                   (assoc :finalGame (to-sql-date (:finalGame v)))
                   util/underscore-keys)))
  (transform transform-player)
  (has-many batting)
  (has-many pitching)
  (has-many fielding))

(defrecord Batting [id
                    player-id
                    year-id
                    stint
                    team-id
                    league-id
                    G ; games
                    AB ; at bats
                    R ; runs
                    H ; hits
                    doubles
                    triples
                    HR ; homeruns
                    RBI ; runs batted in
                    SB ; stolen bases
                    CS ; caught stealing
                    BB ; base on balls
                    SO ; strikeouts
                    IBB ; intentional walks
                    HBP ; hit by pitch
                    SH ; sacrifice hits
                    SF ; sacrifice flies
                    GIDP]) ; grounded into double plays

(defn prepare-batting [v]
  (-> v
      util/underscore-keys
      (util/rename-keys {:doubles :2B, :triples :3B})))

(defn transform-batting [v]
  (-> v
      util/dash-keys
      (util/rename-keys {:2B :doubles, :3B :triples})
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
                     W ; wins
                     L ; losses
                     G ; games
                     GS ; games started
                     CG ; complete games 
                     SHO ; shutouts 
                     SV ; saves
                     IPOuts ; Outs pitched (innings pitched * 3)
                     H ; hits
                     ER ; earned runs
                     HR ; homeruns
                     BB ; walks
                     SO ; strikeouts
                     BAOpp ; opponent's batting average
                     ERA ; earned run average
                     IBB ; intentional walks
                     WP ; wild pitches
                     HBP ; batters hit by pitch
                     BK ; balks
                     BFP ; batters faced by pitcher
                     GF ; games finished
                     R ; runs allowed
                     SH ; sacrifices by opposing batters
                     SF ; sacrifice flies by opposing batters
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

(defrecord Fielding [id
                     player-id
                     year-id
                     stint
                     team-id
                     league-id
                     Pos ; position
                     G ; games
                     GS ; games started
                     InnOuts ; Time played in the field expressed as outs
                     PO ; putouts
                     A ; assists
                     E ; errors 
                     DP ; double plays
                     PB ; passed balls (by catchers)
                     WP ; wild pitches (by catchers)
                     SB ; opponent stolen bases (by catchers)
                     CS ; opponents caught stealing (by catchers)
                     ZR]) ; zone rating

(defentity fielding
  (entity-fields :id :player_id :year_id :stint :team_id :league_id :Pos :G :GS :InnOuts :PO :A :E :DP :PB :WP :SB :CS :ZR)
  (pk :id)
  (prepare (fn [v]
             (-> v
                 util/underscore-keys)))
  (transform (fn [v]
               (-> v
                   util/dash-keys
                   map->Fielding)))
  (belongs-to player))

(defrecord Team [id
                 lahman-id
                 league-id
                 year-id
                 franchise-id
                 division-id
                 rank
                 games-played
                 home-games
                 wins
                 losses
                 division-winner
                 wild-card-winner
                 league-champion
                 world-series-winner
                 runs
                 at-bats
                 hits
                 doubles
                 triples
                 homeruns
                 BB
                 SO
                 SB
                 CS
                 HBP
                 SF
                 RA
                 ER
                 ERA
                 CG
                 SHO
                 SV
                 IPOuts
                 HA
                 HRA
                 BBA
                 SOA
                 E
                 DP
                 FP
                 name
                 park-name
                 attendance
                 BPF
                 PPF
                 baseball-reference-id
                 old-lahman-id
                 retrosheet-id])

(defentity team
  (pk :id)
  (prepare (fn [v]
             (-> v
                 util/underscore-keys)))
  (transform (fn [v]
               (-> v
                   util/dash-keys
                   map->Team)))
  (has-many player))
