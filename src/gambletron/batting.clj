(ns gambletron.batting
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            (gambletron [schema :refer [batting] :as schema]
                        [util :refer [defconst] :as util])))

(defn create [batting-map]
  (insert batting (values batting-map)))

(defn update [batting-map changes]
  (korma/update batting
                (set-fields changes)
                (where {:id (:id batting-map)})))

(defn delete [batting-map]
  (korma/delete batting
                (where {:id (:id batting-map)})))

(defn find-by-id [id]
  (first
   (select batting (where {:id id}))))

(defn find-all-by-player [player-id]
  (select batting (where {:player-id player-id})))

(defn find-all-by-team [team]
  (select batting (where {:team-id (:lahman-id team)
                          :year-id (:year-id team)})))

(defn find-all-by-teams [teams]
  (select batting (where {:team-id [in (set (map :lahman-id teams))]
                          :year-id [in (set (map :year-id teams))]})))

(defconst at-bats :AB)
(defconst hits-by-batters :H)
(defconst walks-by-batters :BB)
(defconst homeruns-by-batters :HR)
(defconst triples (keyword "3B"))

;; how often a batter reaches base
(defn on-base-percentage [{:keys [H BB HBP AB SF] :as batting-map}]
  (/ (+ H BB HBP)
     (+ AB BB HBP SF)))

;; power of a hitter
(defn slugging-percentage [batting-map]
  (+ (hits-by-batters batting-map)
     (:2B batting-map)
     (* 2 (triples batting-map))
     (/ (* 3 (homeruns-by-batters batting-map)) 
        (at-bats batting-map))))

