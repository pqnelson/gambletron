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

(defn batting-avg [batting-map]
  (/ (hits-by-batters batting-map)
     (at-bats batting-map)))

;; how often a batter reaches base
(defn on-base-percentage [{:keys [H BB HBP AB SF] :as batting-map}]
  (/ (+ H BB HBP)
     (+ AB BB HBP SF)))

;; power of a hitter
(defn slugging-percentage [batting-map]
  (/ (+ (hits-by-batters batting-map)
        (* 2 (:2B batting-map))
        (* 3 (:3B batting-map))
        (* 4 (homeruns-by-batters batting-map)))
     (at-bats batting-map)))

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
     (:2B batter)
     (:3B batter)
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

(defn find-latest-for-player [player-id]
  (assoc (->> (select batting (where {:player-id player-id}))
              (sort-by :year-id)
              (take-last 3)
              combine-batting-data)
         :player-id
         player-id))

(defn- get-batting-data* [player-map]
  (assoc
   (combine-batting-data
    (find-all-by-player (:id player-map)))
   :player-id (:id player-map)))

(defonce data-cache (atom {}))

(defn get-batting-data [player-map]
  (let [k (keyword (:id player-map))]
    (if-let [v (@data-cache k)]
      v
      (let [val (get-batting-data* player-map)]
        (swap! data-cache assoc k val)
        val))))

(defn top-sluggers-for-team [team-map]
  (->> team-map
       find-all-by-team
       (filter (comp pos? :AB))
       (sort-by (comp - slugging-percentage))))

(defn top-batting-avg-for-team [team-map]
  (->> team-map
       find-all-by-team
       (filter (comp pos? :AB))
       (sort-by (comp - batting-avg))))

(defn top-obp-for-team [team-map]
  (->> team-map
       find-all-by-team
       (filter (comp pos? :AB))
       (sort-by (comp - on-base-percentage))))

(defn- lineup-order [player-maps]
  [(nth player-maps 1)
   (nth player-maps 4)
   (nth player-maps 2)
   (nth player-maps 5)
   (nth player-maps 3)
   (nth player-maps 6)
   (nth player-maps 7)
   (nth player-maps 8)
   (nth player-maps 0)])

(defn filter-lineup [player-maps]
  (let [lineup-ids (->> player-maps
                        (map get-batting-data)
                        (filter (comp pos? :AB))
                        ;; (sort-by on-base-percentage)
                        ;; (sort-by slugging-percentage)
                        (sort-by batting-avg)
                        (take-last 9)
                        (map :player-id))]
    (lineup-order
     (for [id (reverse lineup-ids)]
       {:id id}))))

(defn no-out-transition-submatrix [batting-data]
  [[(:HR batting-data)
    (+ (walks batting-data)
       (singles batting-data))
    (:2B batting-data)
    (:3B batting-data)
    0
    0
    0
    0]
   [(:HR batting-data)
    0
    0
    (:3B batting-data)
    (+ (walks batting-data)
       (singles batting-data))
    0
    (:2B batting-data)
    0]
   [(:HR batting-data)
    (singles batting-data)
    (:2B batting-data)
    (:3B batting-data)
    (walks batting-data)
    0
    0
    0]
   [(:HR batting-data)
    (singles batting-data)
    (:2B batting-data)
    (:3B batting-data)
    0
    (walks batting-data)
    0
    0]
   [(:HR batting-data)
    0
    0
    (:3B batting-data)
    (singles batting-data)
    0
    (:2B batting-data)
    (walks batting-data)]
   [(:HR batting-data)
    0
    0
    (:3B batting-data)
    (singles batting-data)
    0
    (:2B batting-data)
    (walks batting-data)]
   [(:HR batting-data)
    (singles batting-data)
    (:2B batting-data)
    (:3B batting-data)
    0
    0
    0
    (walks batting-data)]
   [(:HR batting-data)
    0
    0
    (:3B batting-data)
    (singles batting-data)
    0
    (:2B batting-data)
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


(defn transition-matrix [batting-data]
  (let [PA (plate-appearances batting-data)]
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
      {:player-id (:player-id batting-data)})))

(defn league-average [year]
  (when-let [player-ids (seq
                         (set (map :player-id (select schema/rosters
                                                      (where {:year year})))))]
    (let [results (select batting
                          (where {:player-id [in player-ids]
                                  :G [> 20]}))]
      (util/map-vals
       #(double (/ % (count results)))
       (combine-batting-data
        results)))))

;; Produces a 25-by-25 matrix
(defn player->transition-matrix [player-map]
  (transition-matrix (get-batting-data player-map)))
