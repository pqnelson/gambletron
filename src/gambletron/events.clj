(ns gambletron.events
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            (clj-time [coerce :as coerce]
                      [core :as time])
            [korma.core :refer [select where] :as korma]
            [gambletron.schema :refer [events games vw_events vw_games]]
            [gambletron.util :refer [defconst] :as util]
            [gambletron.pitching :as pitching]
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

(defn find-all-since [time-dt]
  (select vw_events
          (where {:game-date [> (coerce/to-sql-date time-dt)]})))

(defn find-start-pitcher-events [game-id]
  (select events
          (where {:game-id game-id
                  :pit-start-fl "T"})))

(defconst national-team-ids ["ARI" "ATL" "CHN" "CIN" "COL" "LAN" "MIA" "MIL" "NYN" "PHI" "PIT" "SDN" "SFN" "SLN" "WAS" "FLO" "MON"])
(defconst american-team-ids ["ANA" "BAL" "BOS" "CHA" "CLE" "DET" "HOU" "KCA" "MIN" "NYA" "OAK" "SEA" "TBA" "TEX" "TOR" "CAL"])

(defn find-foo
  ([]
   (korma/exec-raw [(str "SELECT outs_ct, start_bases_cd, inn_runs_ct, fate_runs_ct, game_id, event_id, bat_id "
                         "FROM vw_events "
                         "WHERE year>2010 "
                        "AND ((away_team_id IN ("
                        (string/join "," (map #(str "'" % "'") american-team-ids))
                        ")) AND (home_team_id IN ("
                        (string/join "," (map #(str "'" % "'") american-team-ids))
                        ")))"
                        )
                   []]
                   :results))
  ([league]
   (korma/exec-raw [(str "SELECT outs_ct, start_bases_cd, inn_runs_ct, fate_runs_ct, game_id, event_id, bat_id "
                         "FROM vw_events "
                         "WHERE year>2010 "
                         "AND ((away_team_id IN ("
                         (string/join "," (map #(str "'" % "'") league))
                         ")) AND (home_team_id IN ("
                         (string/join "," (map #(str "'" % "'") league))
                         ")))"
                         )
                    []]
                   :results)))

(defn- group-into-half-innings [game-events]
  (->> game-events
       (partition-by #(and (zero? (:outs-ct %)) (= "0" (:start-bases-cd %))))
       (partition-all 2)
       (map #(apply concat %))))

(defn- assoc-runs-scored [game-events]
  (for [half-inning (group-into-half-innings game-events)
        event half-inning]
    (assoc event :runs ;; (+ (:inn-runs-ct (last half-inning))
                       ;;    (:fate-runs-ct (last half-inning)))
           (+ 0 ;; (:inn-runs-ct event)
              (:fate-runs-ct event))
           ;; (:fate-runs-ct event)
           )))
       
(defn- remove-duplicate [game-events]
  (loop [[head & remaining] (sort-by :event-id game-events)
         acc []]
    (if (empty? remaining)
      (if head
        (conj acc head)
        acc)
      (if (= (:bat-id head) (:bat-id (first remaining)))
        (recur remaining acc)
        (recur remaining (conj acc head))))))

(defn filter-events [e]
  (->> e
       (map #(assoc % :event-id (util/parse-int (:event-id %))))
       (group-by :game-id)
       (mapcat (fn [[_ game-events]]
                 (assoc-runs-scored (remove-duplicate game-events))))))

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

(defn initial-status [event-map]
  (+ (* 8 (:outs-ct event-map))
     (or (util/parse-int (:start-bases-cd event-map)) 0)))

(defn find-bar []
  (korma/exec-raw [(str "SELECT outs_ct, start_bases_cd, inn_runs_ct, count(*) "
                        "FROM vw_events "
                        "WHERE year>2000 "
                        "GROUP BY outs_ct, start_bases_cd, inn_runs_ct")
                   []]
                  :results))

;; (frequencies (map (juxt initial-status :rbi-ct)

(defn final-status [event-map]
  (+ (* 8 (:event-outs-ct event-map))
     (or (util/parse-int (:end-bases-cd event-map)) 0)))

;; treat various flags as predicates
(defn- flag-predicate [flag-key event-map]
  (when-not (string/blank? (flag-key event-map))
    (= "T" (flag-key event-map))))

(defn pinch-hit? [event-map]
  (flag-predicate :ph-flag event-map))

(defn starter-pitcher? [event-map]
  (flag-predicate :pit-start-fl event-map))

;; helper function to parse chadwick event conditions
(defn- event-type-eq? [code event-map]
  (if (set? code)
    (contains? code (:event-cd event-map))
    (= code (:event-cd event-map))))

;; http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent-eventtype
(defn out? [event-map]
  (event-type-eq? #{2  ; generic out
                    3  ; strikeout
                    6  ; caught stealing
                    8} ; pickoff
                  event-map))

(defn error? [event-map]
  (event-type-eq? #{7   ; pickoff error
                    18} ; error
                  event-map))

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

(defn run-score [event]
  (condp = (:event-cd event)
    20 1
    21 2
    22 3
    23 4
    0))

(defn hit-by-pitch? [event-map]
  (event-type-eq? 16 event-map))

(defn advance? [event-map]
  (or (walk? event-map)
      (hit-by-pitch? event-map)
      (error? event-map)
      (event-type-eq? #{11  ; balk
                        12} ; other advance
                      event-map)))

(defn on-base? [event-map]
  (or (advance? event-map)
      (hit? event-map)))

(defn outcome [event-map]
  (cond
    (on-base? event-map)  99/100
    (out? event-map)       1/100
    :else nil))

(defn assoc-prob [fr]
  (for [row fr
        :let [N (reduce + (map :count (filter #(= ((juxt :outs-ct
                                                     :start-bases-cd) row)
                                              ((juxt :outs-ct
                                                     :start-bases-cd) %))
                                          fr)))
              p (/ (:count row) N)]]
    (dissoc (assoc row :probability (float p)
                   :expected-runs (float (* (:runs row) p)))
            :count)))

(defn parse-bases [base-str]
  (condp = base-str
    "0" "None"
    "1" "(1)"
    "2" "(2)"
    "3" "(1,2)"
    "4" "(3)"
    "5" "(1,3)"
    "6" "(2,3)"
    "7" "(1,2,3)"
    (throw (Error. (str "Unknown base condition: " base-str)))))

(defn make-table [fr]
  (for [[[o b] vs] (group-by (juxt :outs-ct :start-bases-cd) fr)]
    {:outs o
     :bases (parse-bases b)
     :expected-runs (reduce + (map :expected-runs vs))}))


(defconst base-txt ["None" "(1)" "(2)" "(3)" "(1,2)" "(1,3)" "(2,3)" "(1,2,3)"])

(defn make-row [t]
  (string/join "\n"
  (for [outs (range 3)
        :let [row (filter #(= outs (:outs %)) t)]]
    (string/join " | "
                 (cons (str "| " outs)
               (for [base base-txt
                     :let [entry (first (filter #(= base (:bases %)) row))]]
                 (format "%.3f" (float (:expected-runs entry)))))))))

(defn re->table [e]
  (->> e
       filter-events
       (map #(select-keys % [:outs-ct :start-bases-cd :runs]))
       frequencies
       (map (fn [[m n]] (assoc m :count n)))
       assoc-prob
       make-table
       make-row
       println))

(defn avg-fail-rate [pitcher-events]
  (->> pitcher-events
       (partition-by advance?)
       (map count)
       util/mean))

(defn sd-fail-rate [pitcher-events]
  (if-not (some advance? pitcher-events)
    0
    (->> pitcher-events
         (partition-by advance?)
         (map count)
         util/standard-dev)))

(defn make-bf-outs-pairs [events]
  (for [pitcher-id (set (map :pit-id events))
        :let [pitcher-events (sort-by (comp util/parse-int :event-id) (filter #(= (:pit-id %) pitcher-id)
                                     events))]
        :when (> (count pitcher-events) 2)]
    [(count pitcher-events)
     (avg-fail-rate pitcher-events)
     (sd-fail-rate pitcher-events)
     (util/mean (map run-score pitcher-events))]))

(defn foo [g1]
  (mapcat (fn [g] (make-bf-outs-pairs (find-start-pitcher-events
                                       (:game-id g)))) g1))
