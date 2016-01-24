(ns gambletron.stats.logistic
  (:require [clojure.string :as string]
            [incanter.core :as incanter :refer [nrow]]
            [gambletron.stats.glm :refer [glm]]))

;; (defn- input->instance [^doubles xs]
;;   (let [^Instance inst (DenseInstance. (count xs))]
;;     (doseq [[i x] (map list (range) xs)]
;;       (.setValue inst i x))))

(defn regression
  ([y xs]
   (regression y xs (repeat (nrow xs) 0.0) 1e-8))
  ([y xs initial-guess]
   (regression y xs initial-guess 1e-8))
  ([y xs initial-guess tolerance]
   (glm y
        xs
        :family :binomial
        :eps tolerance
        :b-start initial-guess)))

