(ns gambletron.batting
  (:require [clojure.core.matrix :as matrix]
            [korma.core :refer [insert select set-fields where values] :as korma]
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

(defn find-by-year [year]
  (select batting (where {:year-id year})))

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

(defn walks [batting-data]
  (+ (:BB batting-data)
     (:HBP batting-data)
     (:IBB batting-data)))

(defn plate-appearances [batter]
  (+ (at-bats batter)
     (walks batter)
     (:GIDP batter)
     (:SH batter)
     (:SF batter)))

(defn singles [batter]
  (- (hits-by-batters batter)
     (:doubles batter)
     (:triples batter)
     (:HR batter)))

(defn outs [batter]
  (- (plate-appearances batter)
     (hits-by-batters batter)
     (walks batter)))

;;;; Methods related to building the transition matrix for a given player
(defn- combine-batting-data [batting-maps]
  (->> batting-maps
       (map #(dissoc % :id :player-id :year-id :stint :team-id :league-id))
       (apply merge-with +)))

(defn get-batting-data [player-map]
  (assoc
   (combine-batting-data
    (select batting (where {:player-id (:id player-map)})))
   :player-id (:id player-map)))


(defn no-out-transition-submatrix [batting-data]
  [[(:HR batting-data)
    (+ (walks batting-data)
       (singles batting-data))
    (:doubles batting-data)
    (:triples batting-data)
    0
    0
    0
    0]
   [(:HR batting-data)
    0
    0
    (:triples batting-data)
    (+ (walks batting-data)
       (singles batting-data))
    0
    (:doubles batting-data)
    0]
   [(:HR batting-data)
    (singles batting-data)
    (:doubles batting-data)
    (:triples batting-data)
    (walks batting-data)
    0
    0
    0]
   [(:HR batting-data)
    (singles batting-data)
    (:doubles batting-data)
    (:triples batting-data)
    0
    (walks batting-data)
    0
    0]
   [(:HR batting-data)
    0
    0
    (:triples batting-data)
    (singles batting-data)
    0
    (:doubles batting-data)
    (walks batting-data)]
   [(:HR batting-data)
    0
    0
    (:triples batting-data)
    (singles batting-data)
    0
    (:doubles batting-data)
    (walks batting-data)]
   [(:HR batting-data)
    (singles batting-data)
    (:doubles batting-data)
    (:triples batting-data)
    0
    0
    0
    (walks batting-data)]
   [(:HR batting-data)
    0
    0
    (:triples batting-data)
    (singles batting-data)
    0
    (:doubles batting-data)
    (walks batting-data)]])

(defn- one-out-transition-matrix [batting-data]
  [[(outs batting-data) 0 0 0 0 0 0 0]
   [0 (outs batting-data) 0 0 0 0 0 0]
   [0 0 (outs batting-data) (:SF batting-data) 0 0 0 0]
   [(:SF batting-data) 0 0 (outs batting-data) 0 0 0 0]
   [0 0 0 0 (outs batting-data) 0 0 0]
   [0 0 0 0 0 (outs batting-data) 0 0]
   [0 0 0 0 0 (:SF batting-data) (outs batting-data) 0]
   [0 0 0 0 0 0 0 (outs batting-data)]])

(defn- two-out-transition-matrix [{:keys [GIDP] :as batting-data}]
  [[0 0 0 0 0 0 0 0]
   [GIDP 0 0 0 0 0 0 0]
   [GIDP 0 0 0 0 0 0 0]
   [GIDP 0 0 0 0 0 0 0]
   [0 0 GIDP GIDP 0 0 0 0]
   [GIDP GIDP GIDP 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 GIDP 0 0 0 0]])

(defn- zero-to-three-out-submatrix [_]
  [[0] [0] [0] [0] [0] [0] [0] [0]])

(defn- one-to-three-out-submatrix [{:keys [GIDP] :as batting-data}]
  [[0] [GIDP] [0] [GIDP] [GIDP] [GIDP] [0] [GIDP]])

(defn two-to-three-out-submatrix [batting-data]
  (vec (map vector (repeat 8 (outs batting-data)))))

;; TODO: figure out the discrepency between PA & the sum of the rows
(defn- normalize [row-vector]
  (let [sum (reduce + row-vector)]
    (mapv #(/ % sum) row-vector)))

;; Produces a 25-by-25 matrix
(defn transition-matrix [player-map]
  (let [batting-data (get-batting-data player-map)
        PA (plate-appearances batting-data)]
    (with-meta
      (if (zero? PA)
        (matrix/zero-matrix 25 25)
        (mapv normalize
              (vec
               (concat (map (comp vec concat)
                            (no-out-transition-submatrix batting-data)
                            (one-out-transition-matrix batting-data)
                            (two-out-transition-matrix batting-data)
                            (zero-to-three-out-submatrix batting-data))
                       (map (comp vec concat)
                            (vec (repeat 8 (vec (repeat 8 0))))
                            (no-out-transition-submatrix batting-data)
                            (one-out-transition-matrix batting-data)
                            (one-to-three-out-submatrix batting-data))
                       (map (comp vec concat)
                            (vec (repeat 8 (vec (repeat 8 0))))
                            (vec (repeat 8 (vec (repeat 8 0))))
                            (no-out-transition-submatrix batting-data)
                            (two-to-three-out-submatrix batting-data))
                       [(conj (vec (repeat 24 0)) 1)]))))
      {:player-id (:id player-map)})))
