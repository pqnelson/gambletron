(ns gambletron.stats.util)

(defn sq [x] (* x x))

(def sum (partial reduce +))

(defn avg [coll]
  (when (seq coll)
    (->> coll
         (map #(/ % (count coll)))
         sum)))

(defn mode [coll]
  (when (seq coll)
    (->> coll
         frequencies
         (sort-by second >)
         ffirst)))

(defn unit-step
  "Heaviside step function, generalized to arbitrary dimensions. Returns
  0 if any of the xs are negative."
  [& xs]
  (if (some neg? xs)
    0
    1))
