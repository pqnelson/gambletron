(ns gambletron.batter-streaks
  (:require [clj-time.core :as time]
            [clj-time.coerce :as coerce]
            [korma.core :as korma :refer [select where]]
            [gambletron.util :as util]
            [gambletron.retrosheet-schema :refer [events games rosters]]))

(def ^:const event-code-lookup {2 :generic-out
                                3 :strikeout
                                4 :stolen-base
                                5 :defensive-indifferen
                                6 :caught-stealing
                                8 :pickoff
                                9 :wild-pitch
                                10 :passed-ball
                                11 :balk
                                12 :other-advance
                                13 :foul-error
                                14 :nonintentional-walk
                                15 :intentional-walk
                                16 :hit-by-pitcher
                                17 :interference
                                18 :error
                                19 :fielder-choice
                                20 :single
                                21 :double
                                22 :triple
                                23 :homerun})

(defn query-since [since-dt]
  (let [since-int (Long/parseLong (str (time/year since-dt)
                                       (format "%02d" (time/month since-dt))
                                       (format "%02d" (time/day since-dt))))]
    (select events
            (korma/fields (korma/raw "events.bat_id")
                          (korma/raw "events.game_id")
                          (korma/raw "events.event_cd")
                          (korma/raw "events.event_id"))
            (korma/join :inner games (= :games.game_id :events.game_id))
            (where {:games.game-dt [>= since-int]
                    :events.event-cd [not-in ["6" "8" "9" "10" "11" "12"
                                              "13" "17" "18" "19"]]})
            ;; exclude games at Coors field, and those involving rockies
            (where (or {:games.away-team-id [not= "COL"]}
                       {:games.park-id [not= "DEN02"]})))))

(defn- out? [{:keys [event-cd] :as event}]
  (contains? #{2 3} event-cd))

(defn- events->streaks [events]
  (->> events
       (group-by :game-id)
       vals
       (map (fn [game-events]
              (map (complement out?)
                   (sort-by :event-id game-events))))))

(def ^:const minimum-pa 50) ;; arbitrary cutoff

(defn- has-minimum-pa? [[_ vs]]
  (>= (count vs) minimum-pa))

(defn assemble-streaks [since-dt]
  (->> (query-since since-dt)
       (group-by :bat-id)
       seq
       (filter has-minimum-pa?)
       (into {})
       (util/map-vals events->streaks)))

(defn sq [x] (* x x))

(defn singleton? [x]
  (and (coll? x)
       (= 1 (count x))))

(defn player-id->name [id]
  (let [player (first
                (select rosters
                        (where {:player-id id})))]
    (str (:first-name-tx player)
         " "
         (:last-name-tx player)
         " "
         (:team-tx player))))

(defn batter-data->wald-wolfowitz-stat
  "Returns the test statistic for a given batter data, which is
  
    Z = (R - R-bar)/s
    R = observed number of streaks
    R-bar = expected number of streaks
    s = standard deviation on number of streaks
  "
  [[k vs]]
  (let [vs (apply concat vs)
        {hits true, outs false} (frequencies vs)
        at-bats (+ hits outs)
        mu (inc (/ (* 2 hits outs) at-bats))
        sigma (Math/sqrt (/ (* (dec mu) (dec (dec mu)))
                            (dec at-bats)))
        observed (count (partition-by identity vs))]
    (/ (- observed mu) sigma)
    ))

(defn run-wald-wolfowitz-sidalik-tests
  "Returns a hashmap of the batters who pass the Wald-Wolfowitz test,
  and those that fail, specifically `{:pass => [[player-id ww-stat]],
  :fail => [[player-id ww-stat]]}`.
  
  Removes batters who do not have at least 10 hits AND at least 10 outs.
  
  Requires a given significance level for rejecting null
  hypothesis. Adjusts the significance using the Šidák correction.
  
  See:
  - http://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm"
  [data significance-level]
  {:pre [(<= 0 significance-level 1)]}
  (let [remove-too-small-sample (fn [d]
                                  (remove (fn [[_ vs]]
                                            (let [freqs (frequencies
                                                         (apply concat
                                                                vs))]
                                              (or (< (get freqs true 0)
                                                     10)
                                                  (< (get freqs false 0)
                                                     10))))
                                          d))
        data (remove-too-small-sample data)
        alpha (- 1 (Math/pow (- 1 significance-level)
                             (/ 1.0 (count data))))
        z (stats/quantile-normal (- 1 (/ alpha 2)))
        ]
    (reduce (fn [acc [k vs]]
              (let [stat (Math/abs (batter-data->wald-wolfowitz-stat
                                [k vs]))]
                (if (> stat z)
                  (update acc :fail conj [k stat])
                  (update acc :pass conj [k stat]))))
            {:pass []
             :fail []}
            data)))

(defn run-wald-wolfowitz-tests
  "Returns a hashmap of the batters who pass the Wald-Wolfowitz test,
  and those that fail, specifically `{:pass => [[player-id ww-stat]],
  :fail => [[player-id ww-stat]]}`.
  
  Removes batters who do not have at least 10 hits AND at least 10 outs.
  
  Requires a given significance level for rejecting null
  hypothesis. Uses the Holm–Bonferroni correction for family-wise
  hypothesis testing.
  
  See:
  - http://www.itl.nist.gov/div898/handbook/eda/section3/eda35d.htm
  - https://en.wikipedia.org/wiki/Holm%E2%80%93Bonferroni_method"
  [data significance-level]
  {:pre [(<= 0 significance-level 1)]}
  (let [remove-too-small-sample (fn [d]
                                  (remove (fn [[_ vs]]
                                            (let [freqs (frequencies
                                                         (apply concat
                                                                vs))]
                                              (or (< (get freqs true 0)
                                                     10)
                                                  (< (get freqs false 0)
                                                     10))))
                                          d))
        data (remove-too-small-sample data)
        alpha significance-level
        z (stats/quantile-normal (- 1 (/ alpha 2)))
        ]
    (->> (seq data)
         (map (fn [[k vs]]
                [k (batter-data->wald-wolfowitz-stat [k vs])]))
         (sort-by  second)
         (map list (rest (range)))
         (take-while (fn [[j [key stat]]]
                       (> (- 1 (stats/cdf-normal stat))
                          (/ alpha (- (inc (count data)) j)))))
         )))

(defn holm-bonferroni [alpha stats]
  (->> stats
       (sort-by second)
       (map list (rest (range)))
       (take-while (fn [[j [key p-value]]]
                     (> p-value
                        (/ alpha (- (inc (count stats)) j)))))))

(defn sidalik [alpha stats]
  (let [adj-alpha (- 1 (Math/pow (- 1 alpha) (/ 1 (count stats))))]
    (->> stats
         (remove (fn [[k p-value]]
                   (< p-value
                      adj-alpha))))))

(defonce binomial-cache (atom {}))
(defn binomial [n k]
  (cond
    (= n k)   1
    (zero? k) 1
    (zero? n) 0
    (> k n)   0
    (neg? k)  0
    :else (or (get @binomial-cache [n k])
              (try (let [rprod (fn [a b] (reduce *' (range a (inc b))))
                         x (/ (rprod (- n k -1) n) (rprod 1 k))]
                     (swap! binomial-cache assoc [n k] x)
                     x)
                   (catch Exception e (do (println "failed on [n,k] = ["
                                                   n "," k "]"
                                                   )
                                          (throw e)))))))

(defn- small-wald-wolfowitz-stat* [hits outs streaks]
  (let [plate-appearances (+ hits outs)
        probability (fn [runs]
                      (let [k (quot runs 2)]
                        (if (even? runs)
                          (/ (*' 2.0
                                (binomial (dec hits) (dec k))
                                (binomial (dec outs) (dec k)))
                             (binomial plate-appearances hits))
                          (double (/ (+' (*' (binomial (dec hits) k)
                                   (binomial (dec outs) (dec k)))
                                (*' (binomial (dec hits) (dec k))
                                   (binomial (dec outs) k)))
                             (binomial plate-appearances hits))))))]
    (reduce (fn [acc k]
              (+ acc
                 (probability k)))
            0
            (range 2 (inc streaks)))))

(defonce ww-stat-cache (atom {}))

(defn small-wald-wolfowitz-stat [hits outs streaks]
  (or (get @ww-stat-cache [hits outs streaks])
      (let [x (small-wald-wolfowitz-stat* hits outs streaks)]
        (swap! ww-stat-cache assoc [hits outs streaks] x)
        x)))

(defn- ww-p-value-for-game [game-hits]
  (let [plate-appearances (count game-hits)
        hits (count (filter identity game-hits))
        outs (- plate-appearances hits)
        streaks (count (partition-by identity game-hits))]
    (double (small-wald-wolfowitz-stat hits outs streaks))))

(defn pairwise-wald-wolfowitz-tests [data]
  (->> (seq data)
       (mapcat (fn [[k vs]]
                 (->> vs
                      (remove singleton?)
                      (map (fn [v] [k (ww-p-value-for-game v)])))))
       (filter identity)
       (remove (fn [[k v]]
                 (Double/isNaN v)))))

(defn slow-wald-wolfowitz-tests [data]
  (->> data
       (map (fn [[k vs]]
              [k (ww-p-value-for-game (apply concat vs))]))
       (remove (fn [[k v]]
                 (Double/isNaN v)))))

(defonce data-cache (atom {}))
(defn data-for-year [year]
  (let [k (keyword (str year))]
    (or (k @data-cache)
        (let [data (assemble-streaks (time/date-time year))]
          (swap! data-cache assoc k data)
          data))))

(defn data-2006 [] (data-for-year 2006))
(defn data-2014 [] (data-for-year 2014))

;; The proposition functions will give us the proportion of the
;; population which fail to reject the null hypothesis (i.e., "appear to
;; be independent")
(defn proposition-1 []
  (let [alpha 0.05
        ww-stats (slow-wald-wolfowitz-tests (data-2014))]
    {:exact {:holm-bonferroni (double (/ (count (holm-bonferroni alpha ww-stats))
                                         (count ww-stats)))
             :sidalik (double (/ (count (sidalik alpha ww-stats))
                                 (count ww-stats)))}
     :normal-approx {:holm-bonferroni (double (/ (count (run-wald-wolfowitz-tests
                                                         (data-2014) alpha))
                                                 (count (data-2014))))
                     :sidalik (double
                               (/ (count (:pass (run-wald-wolfowitz-sidalik-tests
                                                 (data-2014)
                                                 alpha)))
                                  (count (data-2014))))}
     }))

(defn proposition-2 []
  (let [alpha 0.05
        ww-stats (->> (data-2006)
                      slow-wald-wolfowitz-tests
                      (sort-by second)
                      (remove (comp zero? second)))
        ]
    {:exact {:holm-bonferroni (double (/ (count (holm-bonferroni alpha ww-stats))
                                         (count ww-stats)))
             :sidalik (double (/ (count (sidalik alpha ww-stats))
                                 (count ww-stats)))}
     :normal-approx {:holm-bonferroni (double
                                       (/ (count
                                           (run-wald-wolfowitz-tests
                                            (data-2006)
                                            alpha))
                                          (count (data-2006))))
                     :sidalik (double
                               (/ (count (:pass (run-wald-wolfowitz-sidalik-tests
                                                 (data-2006)
                                                 alpha)))
                                  (count (data-2006))))}
     }))

(defn proposition-3 []
  (let [alpha 0.05
        ww-stats (->> (data-2014)
                      pairwise-wald-wolfowitz-tests
                      (remove (comp zero? second))
                      (sort-by second))]
    {:holm-bonferroni (double (/ (count (holm-bonferroni alpha ww-stats))
                                 (count ww-stats)))
     :sidalik (double (/ (count (sidalik alpha ww-stats))
                         (count ww-stats)))}))

(defn proposition-4 []
  (let [alpha 0.05
        ww-stats (->> (data-2006)
                      pairwise-wald-wolfowitz-tests
                      (remove (comp zero? second))
                      (sort-by second))]
    {:holm-bonferroni (double (/ (count (holm-bonferroni alpha ww-stats))
                                 (count ww-stats)))
     :sidalik (double (/ (count (sidalik alpha ww-stats))
                         (count ww-stats)))}))

