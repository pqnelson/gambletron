(ns gambletron.stats.glm
  (:require [incanter.core :as incanter :refer [diag div exp length log
                                                minus mmult mult ncol nrow plus
                                                solve to-vect trans]]
            [incanter.stats :as stats :refer [euclidean-distance]]))
;; For details, see http://bwlewis.github.io/GLM/

(def ^:dynamic *distance* euclidean-distance)
(def forward-solve incanter/solve)
(def back-solve incanter/solve)

(defstruct family :link :inverse-link :d-link :weight)
(def families
  {:gaussian (struct-map family
                         :link (fn [x] x)
                         :inverse-link (fn [x] x)
                         :d-link (fn [x] 1)
                         :weight (fn [mu] (repeat (length mu) 1)))
   :binomial (struct-map family
                         :link (fn [x] (log (div x (minus 1 x))))
                         :inverse-link (fn [x] (div (exp x) (plus 1 (exp x))))
                         :d-link (fn [x] [x] (div 1 (mult x (minus 1 x))))
                         :weight (fn [mu] (to-vect (mult mu (minus 1 mu)))))})

(defn- safe-div [numerator denominator]
  (if (zero? denominator)
    0
    (/ numerator denominator)))

;; Iteratively reweighted least squares
;; NB: z = eta + (b - mu)/(d-mu) but calculus shows this is equal to
;;     z = eta + (b - mu)*(d-link mu)
(defn iter-reweighted-least-squares
  [y X initial-guess inverse-link d-link weight eps]
  (let [reweigh-least-squares (fn [B-next]
                                (let [eta (mmult X B-next)
                                      mu (inverse-link eta)
                                      d-mu (safe-div 1.0
                                                     (d-link mu))
                                      z (plus eta (mmult (minus y mu)
                                                         (d-link mu)))
                                      W (diag (div (mult d-mu d-mu)
                                                   (stats/variance mu)))]
                                  (mmult
                                   (solve (mmult (trans X) W X))
                                   (trans X) W z)))]
    (last
     (last
      (take-while
       (fn [[guess next-guess]] (> (*distance* guess next-guess)
                                  eps))
       (partition 2 1 (iterate reweigh-least-squares initial-guess)))))))


(defn cholesky-reweighted-least-squares
  ([y X initial-guess inverse-link d-link weight eps]
   (cholesky-reweighted-least-squares
    y X initial-guess inverse-link d-link weight eps 25))
  ([y X initial-guess inverse-link d-link weight eps max-iter]
   (let [{q-mat :Q r-mat :R} (incanter/decomp-qr X)
         initial-t (repeat (length y) 0.0)]
     (cholesky-reweighted-least-squares
      y X initial-guess inverse-link d-link weight eps initial-t q-mat r-mat)))
  ([y X initial-guess inverse-link d-link weight eps max-iter old-t q-mat r-mat]
   (let [mu (inverse-link old-t)
         d-t (safe-div 1.0
                        (d-link old-t))
         z (plus old-t (mmult (minus y mu)
                              (d-link mu)))
         W (diag (div (mult d-t d-t)
                      (stats/variance mu)))
         {:keys [L L*]} (incanter/decomp-cholesky (mmult (trans q-mat) W q-mat))
         s (mmult (forward-solve L)
                  (trans q-mat)
                  W
                  z)
         s (mmult (back-solve L*) s)
         t (mmult q-mat s)]
     (if (or (<= max-iter 0)
             (< (*distance* (minus s initial-guess)) eps))
       (mmult (back-solve r-mat) (trans q-mat) t)
       (recur
        y X s inverse-link d-link weight eps (dec max-iter) t q-mat r-mat)))))

(defn glm [y X & {:keys [family intercept eps b-start]
                  :or {:family :gaussian
                       :intercept true
                       :eps 0.01
                       :b-start (repeat (if intercept
                                          (inc (ncol X))
                                          (ncol X))
                                        0.0)}}]
  (let [family (families family)]
    (iter-reweighted-least-squares
     y
     (if intercept
       (incanter/bind-columns (repeat (nrow X) 1.0) X)
       X)
     b-start
     (:inverse-link family)
     (:d-link family)
     (:weight family)
     eps)))
