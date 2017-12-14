(ns gambletron.retrosheet-schema
  (:require (clj-time [coerce :as coerce]
                      [core :as time]
                      [format :as format])
            [clojure.string :as string]
            [korma.core :refer :all :as korma]
            [korma.db :refer :all]
            [gambletron.util :refer [defconst] :as util]))

(declare events)

(def db-dir (str (System/getProperty "user.dir") "/resources/"))

(defn- underscores->dashes [n]
  (-> n util/snake->kabob keyword))

(defn- dashes->underscores [n]
  (-> n name util/kabob->snake))

(def dash-naming-strategy {:keys underscores->dashes
                           :fields dashes->underscores})

;;(defdb db (postgres {:db "gambletron"
;;                     :user "gambletron"
;;                     :password "gambletron"
;;                     :naming dash-naming-strategy}))

(defdb db (postgres {:db "retrosheet"
                     :user "wells"
                     :password "wells"
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
                   map->Team))))


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

