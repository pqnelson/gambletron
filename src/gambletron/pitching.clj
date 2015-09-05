(ns gambletron.pitching
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [pitching player] :as schema]
            [gambletron.util :refer [defconst] :as util]
            [gambletron.stats.util :as stats-util]))

(defn create [pitching-map]
  (insert pitching (values pitching-map)))

(defn update [pitching-map changes]
  (korma/update pitching
                (set-fields changes)
                (where {:id (:id pitching-map)})))

(defn delete [pitching-map]
  (korma/delete pitching
                (where {:id (:id pitching-map)})))

(defn find-by-id [id]
  (first
   (select pitching (where {:id id}))))

(defn find-by-player [player-id]
  (select pitching (where {:player-id player-id})))

(defn find-by-player-year [player-id year]
  (first
   (select pitching (where {:player-id player-id
                            :year-id year}))))

(defn find-by-team-id [team-id]
  (select pitching (where {:team-id team-id})))

(defn find-by-team [team]
  (find-by-team-id (:lahman-id team)))

(defn find-by-team-year [team]
  (select pitching (where {:team-id (:lahman-id team)
                           :year-id (:year-id team)})))


(defn find-by-year [year]
  (select pitching (where {:year-id year})))

;; how many batters hit the ball, and then do something
;; compared to the total number of batters that have faced the pitcher
(defn hit-rate [{:keys [BB HR H SO IBB]}]
  (if (every? zero? [BB HR H SO IBB])
    0
    (/ (+ HR H) (+ BB HR H SO IBB))))

(defconst batters-faced :BFP)

;; (defn batters-faced [{:keys [IPOuts H BB]}]
;;   (+ IPOuts H BB))

(defn obp [{:keys [H BFP BB HBP SH]}]
  (/ (+ H BB HBP)
     (+ BFP H HBP SH)))

(defn combine-data [pitching-maps]
  (apply merge-with +
         (map #(dissoc % :id :player-id :year-id :stint :team-id :league-id)
              pitching-maps)))

(defn- get-data* [player-map]
  (assoc
   (combine-data
    (find-by-player (:id player-map)))
   :player-id (:id player-map)))

(def get-data (util/soft-memoize get-data*))

;; Returns the leage average on-base-percentage for the current year
(defn league-average-obp* [pitching-map]
  (->> (select pitching (where {:year-id (:year-id pitching-map)
                                :league-id (:league-id pitching-map)}))
       (filter (comp pos? :BFP))
       (map obp)
       stats-util/avg))

(def league-average-obp (util/soft-memoize league-average-obp*))
