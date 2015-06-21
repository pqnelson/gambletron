(ns gambletron.team
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [team] :as schema]))

(defn create [team-map]
  (insert team (values team-map)))

(defn update [team-map changes]
  (korma/update team
                (set-fields changes)
                (where {:id (:id team-map)})))

(defn delete [team-map]
  (korma/delete team
                (where {:id (:id team-map)})))

(defn find-all []
  (select team))

(defn find-by-id [id]
  (first
   (select team (where {:id id}))))

(defn find-by-lahman-id [lahman-id]
  (first
   (select team (where {:lahman-id lahman-id
                        :year-id 2014}))))

(defn find-by-lahman-ids [lahman-ids]
  (select team (where {:lahman-id [in lahman-ids]}))) 

(defn find-since [year]
  (select team (where {:year-id [> year]})))

(defn find-current []
  (select team (where {:year-id 2014})))

(defn current-team-lahman-ids []
  (map :lahman-id
       (select team (where {:year-id 2014}))))

(defn runs-per-game [team-map]
  (/ (:runs team-map)
     (:games-played team-map)))

(defn runs-per-game-for-team-since [lahman-id start-year]
  (into {}
        (map (juxt :year-id (comp float runs-per-game))
             (select team (where {:lahman-id lahman-id
                                  :year-id [>= start-year]})))))

(defn runs-per-game-for-team-hash [start-year]
  (into {}
        (for [id (current-team-lahman-ids)]
          [id (runs-per-game-for-team-since id start-year)])))

(defn mean [coll]
  (when (seq coll)
    (/ (reduce + coll) (count coll))))

(defn sq [x] (* x x))

(defn standard-dev [coll]
  (when (seq coll)
    (let [mu (mean coll)]
      (Math/sqrt
       (double
        (/ (reduce + (map (fn [x] (sq (- x mu))) coll))
           (count coll)))))))

(defn run-data-for-teams-since [start-year]
  (into {}
        (for [id (current-team-lahman-ids)
              :let [data (runs-per-game-for-team-since id start-year)
                    avg (mean (vals data))
                    sd (standard-dev (vals data))]]
          [id {:avg avg, :sd sd}])))

(defn batting-avg [team-map]
  (/ (:hits team-map)
     (:at-bats team-map)))

;; how often a batter reaches base
(defn on-base-percentage [team]
  (/ (+ (or (:hits team) 0)
        (or (:BB team) 0)
        (or (:HBP team) 0))
     (+ (or (:at-bats team) 0)
        (or (:BB team) 0)
        (or (:SF team) 0)
        (or (:HBP team) 0))))

(defn slugging-percentage [team]
  (+ (:hits team)
     (:doubles team)
     (* 2 (:triples team))
     (/ (* 3 (:homeruns team)) 
        (:at-bats team))))
