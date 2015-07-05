(ns gambletron.pitching
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [pitching] :as schema]))

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

(defn batters-faced [{:keys [IPOuts H BB]}]
  (+ IPOuts H BB))
