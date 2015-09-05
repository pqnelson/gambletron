(ns gambletron.events
  (:require [korma.core :refer [select where] :as korma]
            [gambletron.schema :refer [events]]
            [gambletron.util :refer [defconst] :as util]
            [gambletron.player :as player]
            [gambletron.stats.util :as stats-util]))

(defn find-all-by-batter-id [player-id]
  (select events
          (where {:bat-id player-id})))

(defn find-all-by-pitcher-id [player-id]
  (select events
          (where {:pit-id player-id})))

(defn find-all-by-game-id [game-id]
  (select events
          (where {:game-id game-id})
          (korma/order :event-id :asc)))

(defconst get-batter-id :bat-id)
(defconst get-pitcher-id :pit-id)

(defn get-batter [event-map]
  (when-let [batter-id (:bat-id event-map)]
    (player/find-by-id batter-id)))

(defn get-pitcher [event-map]
  (when-let [pitcher-id (:pit-id event-map)]
    (player/find-by-id pitcher-id)))

(defn year [event-map]
  (read-string (subs (:game-id event-map) 3 7)))

;; helper function to parse chadwick event conditions
(defn- event-type-eq? [code event-map]
  (if (set? code)
    (contains? code (:event-cd event-map))
    (= code (:event-cd event-map))))

;; http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent-eventtype
(defn out? [event-map]
  (event-type-eq? #{2 3 6 7 8} event-map))

(defn hit? [event-map]
  (event-type-eq? #{20 ; single
                    21 ; double
                    22 ; triple
                    23}
                  event-map))

(defn homerun? [event-map]
  (event-type-eq? 23 event-map))

(defn walk? [event-map]
  (event-type-eq? #{14 15} event-map))

(defn hit-by-pitch? [event-map]
  (event-type-eq? 16 event-map))

(defn advance? [event-map]
  (event-type-eq? #{11  ; balk
                    12  ; other advance
                    14  ; walk
                    15  ; intentional walk
                    16  ; hit by pitch
                    18} ; error
                  event-map))

(defn on-base? [event-map]
  (or (advance? event-map)
      (hit? event-map)))

(defn outcome [event-map]
  (cond
    (on-base? event-map)  99/100
    (out? event-map)       1/100
    :else nil))

(defn- get-league-avg* [event-map]
  (let [p (pitching/find-by-player-year (get-pitcher-id event-map)
                                        (year event-map))]
    (pitching/league-average-obp p)))

(def get-league-avg (util/soft-memoize get-league-avg*))

(defn inputs [event-map]
  [(double (e->obp event-map))
   (double (e->obpa event-map))
   (double (get-league-avg event-map))])

;; (def lm1 (stats/linear-model (map (comp stats-util/logit outcome) e2) (map inputs e2) :intercept false))
