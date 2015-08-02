(ns gambletron.team
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [team] :as schema]
            [gambletron.util :refer [mean standard-dev]]))

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

(defonce current-teams-cache (atom nil))

(defn find-current []
  (or
   @current-teams-cache
   (reset! current-teams-cache
           (select team (where {:year-id 2014})))))

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

(defn run-data-for-teams-since [start-year]
  (into {}
        (for [id (current-team-lahman-ids)
              :let [data (runs-per-game-for-team-since id start-year)
                    avg (mean (vals data))
                    sd (standard-dev (vals data))]]
          [id {:avg avg, :sd sd}])))

;; this differs slightly from BA in that it includes home runs
(defn hit-rate [{:keys [hits homeruns SO BB]}]
  (if (every? zero? [hits homeruns SO BB])
    0
    (/ (+ hits homeruns) (+ SO BB hits homeruns))))

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

(defn batters-faced [{:keys [IPOuts hits BB]}]
  (+ IPOuts hits BB))

(defn league-average-hits []
  (let [current-teams (find-current)]
    (/ (reduce + (map :hits current-teams))
       (reduce + (map batters-faced current-teams)))))

(defn plate-appearances [team-map]
  (+ (:at-bats team-map)
     (:BB team-map)
     (:HBP team-map)))

(defn outs [team-map]
  (- (plate-appearances team-map)
     (:hits team-map)
     (:BB team-map)
     (:HBP team-map)))

(defn bb [team-map]
  (+ (:BB team-map)
     (:HBP team-map)))

(defn h1 [team-map]
  (- (:hits team-map)
     (:doubles team-map)
     (:triples team-map)
     (:homeruns team-map)))

(def h2 :doubles)
(def h3 :triples)
(def hr :homeruns)

(defn national-league? [team-map]
  (= "NL" (:league-id team-map)))

(defn american-league? [team-map]
  (= "AL" (:league-id team-map)))

(defn designated-hitter-rule? [team-map]
  (american-league? team-map))
