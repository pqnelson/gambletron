(ns gambletron.markov
  (:require [clojure.core.matrix :as matrix]
            [clojure.math.combinatorics :as combinatorics]
            [taoensso.timbre.profiling :as profiling :refer (pspy pspy* profile defnp p p*)]
            (gambletron [batting :as batting]
                        [pitching :as pitching]
                        [player :as player]
                        [team :as team]
                        [util :refer [sum] :as util])))

(defn- row-zero? [row]
  (p :row-zero? (every? zero? row)))

(defn- degenerate? [transition-matrix]
  (p :degenerate? (some row-zero? transition-matrix)))

(defn- make-pseudo-player [player-id]
  {:id player-id})

(defn- remove-pitchers [team player-ids]
  (p :remove-pitchers
     (if-not (team/designated-hitter-rule? team)
       player-ids
       (remove (set (map :player-id (pitching/find-by-team-year team)))
               player-ids))))

(defn- players-for-team [team-map]
  (p :players-for-team
     (->> team-map
          batting/find-all-by-team
          (map :player-id)
          set
          (remove-pitchers team-map)
          (map make-pseudo-player))))

(defn transition-matrices-for-team [team-map]
  (p :transition-matrices-for-team
     (->> team-map
          players-for-team
          batting/filter-lineup
          (map batting/transition-matrix))))

(defn- components-from-row [row components]
  (p :components-from-row
     (vec
      (if (empty? components)
        (repeat 25 0)
        (for [[i val] (map list (range) row)]
          (if (contains? (set components) i)
            val
            0))))))

(def initial-state (vec (cons 1 (repeat 24 0))))
                     
(defn- zero-run-submatrix [transition-mat]
  (p :zero-run-submatrix
     (mapv components-from-row transition-mat
           [[1 2 3 8]
            [4 6 9 16]
            [4 10 11 16]
            [5 11 16]
            [4 7 12 18 19]
            [7 9 13]
            [7 14]
            [12 15]
            [9 10 11 16]
            [12 14 17 24]
            [12 18 19]
            [13 19 24]
            [15 20 24]
            [12 15 17 21 24]
            [15 21 22]
            [23 24]
            [17 18 19 24]
            [20 22 24]
            [20 24]
            [21 24]
            [23 24]
            [23 24]
            [23 24]
            [24]
            [24]])))
(defn- one-run-submatrix [transition-mat]
  (p :one-run-submatrix
     (mapv components-from-row transition-mat
           [[0]
            [3]
            [1 2 3]
            [1 2 3 8]
            [6]
            [4 6 16 17 18]
            [13]
            [7 19]
            [8]
            [11]
            [10 11 9]
            [10 11 9 16]
            [12 14]
            [14]
            []
            [15 20]
            [16]
            [19]
            [17 18 19]
            [17 18 19]
            [20 22]
            [20 22]
            []
            [23]
            []])))

(defn- two-run-submatrix [transition-mat]
  (p :two-run-submatrix
     (mapv components-from-row transition-mat
           [[]
            [0]
            [0]
            [0]
            [3]
            [3]
            [1 2 3]
            [4 6]
            []
            [8]
            [8]
            [8]
            [11]
            [11]
            [9 10 11]
            [12 14]
            []
            [16]
            [16]
            [16]
            [19]
            [16 19]
            [17 18 19]
            [20 22]
            []])))
(defn- three-run-submatrix [transition-mat]
  (p :three-run-submatrix
     (mapv components-from-row transition-mat
           [[]
            []
            []
            []
            [0]
            [0]
            [0]
            [3]
            [4]
            []
            []
            []
            [8]
            [8]
            [8]
            [11]
            []
            []
            []
            []
            [16]
            []
            [16]
            [19]
            []])))
(defn- four-run-submatrix [transition-mat]
  (p :four-run-submatrix
     (mapv components-from-row transition-mat
           [[]
            []
            []
            []
            []
            []
            []
            [0]
            []
            []
            []
            []
            []
            []
            []
            [8]
            []
            []
            []
            []
            []
            []
            []
            [16]
            []])))

(defn find-transition-matrices-for-team [team-map]
  (p :find-transition-matrices-for-team
     (->> team-map
          players-for-team
          (map batting/transition-matrix))))

(defn- safe-mult [row-index current-state run-matrix]
  (p :safe-mult
     (if (row-zero? (nth current-state row-index))
       (nth current-state row-index)
       (matrix/mmul (nth current-state row-index)
                    run-matrix))))

(defn- compute-row [current-state transition-matrix]
  (p :compute-row
     (let [zero-runs (zero-run-submatrix transition-matrix)
           one-runs (one-run-submatrix transition-matrix)
           two-runs (two-run-submatrix transition-matrix)
           three-runs (three-run-submatrix transition-matrix)
           four-runs (four-run-submatrix transition-matrix)]
       (fn [row-index]
         (if (> row-index 3)
           (matrix/add (safe-mult row-index current-state zero-runs)
                       (safe-mult (- row-index 1) current-state one-runs)
                       (safe-mult (- row-index 2) current-state two-runs)
                       (safe-mult (- row-index 3) current-state three-runs)
                       (safe-mult (- row-index 4) current-state four-runs))
           (condp = row-index
             3 (matrix/add (safe-mult row-index current-state zero-runs)
                           (safe-mult (- row-index 1) current-state one-runs)
                           (safe-mult (- row-index 2) current-state two-runs)
                           (safe-mult (- row-index 3) current-state three-runs))
             2 (matrix/add (safe-mult row-index current-state zero-runs)
                           (safe-mult (- row-index 1) current-state one-runs)
                           (safe-mult (- row-index 2) current-state two-runs))
             1 (matrix/add (safe-mult row-index current-state zero-runs)
                           (safe-mult (- row-index 1) current-state one-runs))
             0 (safe-mult row-index current-state zero-runs)
             (IndexOutOfBoundsException. (str "Attempting to compute row "
                                              row-index))))))))

(defn- pivot->update-matrix [pivot-vec]
  (p :pivot->update-matrix
     (let [last-column (concat (map - pivot-vec) (repeat 21 0))
           first-column (concat (repeat 21 0) pivot-vec)]
       (vec
        (for [[s e] (map list first-column last-column)]
          (vec
           (concat [s] (repeat 23 0) [e])))))))

(defn update-innings [state-matrix]
  (p :update-innings
     (let [pivot (drop-last 21 (mapv last state-matrix))]
       (if (row-zero? pivot)
         state-matrix
         (matrix/add state-matrix
                     (pivot->update-matrix pivot))))))

(defn next-state [current-state transition-matrix]
  (p :next-state
     (let [f (compute-row current-state transition-matrix)]
       (mapv f (range (count current-state))))))

(def initial-game-state (vec (cons initial-state
                                   (repeat (- (* 9 21) 1)
                                           (vec (repeat 25 0))))))
(def overtime-game-state (vec (cons initial-state
                                    (repeat 20
                                            (vec (repeat 25 0))))))
(def training-game-state (vec (cons initial-state
                                    (repeat 20 ;;(- (* 1 21) 1)
                                            (vec (repeat 25 0))))))


(defn- done? [state-matrix]
  (p :done?
     (> (sum (take-last 21 (map last state-matrix)))
        0.998)))

(defn compute-for-lineup
  "Produces the distribution of runs for the team, in the form of a
  vector. So `(nth (compute-for-team my-team) 3)` would get the
  probability of scoring (- 3 1) points."
  [transition-matrices game-state-matrix]
  (p :compute-for-lineup
     (loop [[batter & remaining-team] (cycle transition-matrices)
            state-matrix game-state-matrix
            k 0]
       (if (or (> k 150) (done? state-matrix))
         (->> (next-state state-matrix (first remaining-team))
              update-innings
              (map last)
              (take-last 21))
         (recur remaining-team
                (update-innings (next-state state-matrix batter))
                (inc k))))))

(defn compute-for-team
  "Produces the distribution of runs for the team, in the form of a
  vector. So `(nth (compute-for-team my-team) 3)` would get the
  probability of scoring (- 3 1) points."
  [team-map]
  (println (:name team-map))
  (p :compute-for-team
     (compute-for-lineup (transition-matrices-for-team team-map) initial-game-state)))

(defn overtime-distr [team-map]
  (p :compute-for-team
     (loop [[batter & remaining-team] (cycle (transition-matrices-for-team team-map))
            state-matrix overtime-game-state
            k 0]
       (if (or (> k 100) (done? state-matrix))
         (map last state-matrix)
         (recur remaining-team
                (update-innings (next-state state-matrix batter))
                (inc k))))))

(defn- probability-win
  "Returns the probability team-a will win against team-b without extra innings"
  [team-a-point-distr team-b-point-distr]
  (sum
   (map *
        (rest team-a-point-distr)
        (drop-last (reductions + team-b-point-distr)))))

(defn- overtime-probability [team-a-point-distr team-b-point-distr]
  (sum (map * team-a-point-distr team-b-point-distr)))

(defn expected-number-of-runs [point-distr]
  (sum (map * point-distr (range))))

(defn run-variance [point-distr]
  (let [mu (expected-number-of-runs point-distr)]
    (/ (- (sum (map * point-distr (range) (range)))
          (* mu mu))
       21)))

(defn predict [team-a team-b]
  (let [team-a-distr (compute-for-team team-a)
        team-b-distr (compute-for-team team-b)
        team-a-overtime-distr (overtime-distr team-a)
        team-b-overtime-distr (overtime-distr team-b)
        prob-overtime (overtime-probability team-a-distr team-b-distr)
        prob-super-overtime (overtime-probability team-a-overtime-distr
                                                  team-b-overtime-distr)]
    {:probability-team-a-win
     (+ (probability-win team-a-distr team-b-distr)
        (* prob-overtime
           (probability-win team-a-overtime-distr team-b-overtime-distr))
        (* (/ (* prob-overtime prob-super-overtime)
              (- 1 prob-super-overtime))
           (probability-win team-a-overtime-distr
                             team-b-overtime-distr)))
     :extra-innings? (and (< (probability-win team-a-distr team-b-distr) 0.5)
                          (< (probability-win team-b-distr team-a-distr) 0.5))
     :team-a-score (+ (expected-number-of-runs team-a-distr)
                      (expected-number-of-runs
                       (mapv +
                             (mapv #(* (Math/sqrt prob-overtime) %)
                                   team-a-overtime-distr)
                             (mapv #(* (Math/sqrt (/ (* prob-overtime prob-super-overtime)
                                                     (- 1 prob-super-overtime))) %)
                                   team-a-overtime-distr))))
     :team-b-score (+
                    (expected-number-of-runs team-b-distr)
                    (expected-number-of-runs
                     (mapv +
                          (mapv #(* (Math/sqrt prob-overtime) %)
                                team-b-overtime-distr)
                          (mapv #(* (Math/sqrt (/ (* prob-overtime prob-super-overtime)
                                                  (- 1 prob-super-overtime))) %)
                                team-b-overtime-distr))))}))
;;; trying to optimize batting order
(defn- lineup-candidates [team-map]
  (->> (concat (take 9 (batting/top-obp-for-team team-map))
               (take 9 (batting/top-batting-avg-for-team team-map))
               (take 9 (batting/top-sluggers-for-team team-map)))
       set
       (map (comp make-pseudo-player :player-id))
       (#(combinatorics/combinations % 9))))

(defn- runs-for-lineup [batting-lineup]
  (->> batting-lineup
       (map batting/transition-matrix)
       (#(compute-for-lineup % initial-game-state))
       expected-number-of-runs))

;; 220 combinations, 1 inning: 253 seconds
;; 2002 combinations, 1 inning: 3187 seconds
;; 220 combinations, 2 innings: 791 seconds
;; 220 combinations, 3 innings: 1547 seconds
(defn- optimal-batting-order [team-map]
  (->> team-map
       lineup-candidates
       (sort-by (comp - runs-for-lineup))
       first))

(def optimal-LAA-order [{:id "hamiljo03"}
                        {:id "troutmi01"}
                        {:id "kendrho01"}
                        {:id "iannech01"}
                        {:id "pujolal01"}
                        {:id "cowgico01"}
                        {:id "croncj01"}
                        {:id "stewaia01"}
                        {:id "calhoko01"}])
