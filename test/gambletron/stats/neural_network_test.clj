(ns gambletron.stats.neural-network-test
  (:require [clojure.test :refer :all]
            [gambletron.stats.neural-network :refer :all]))

(deftest xor-neural-net-test
  (let [xor-dataset [[[0 1] [1]]
                     [[0 0] [0]]
                     [[1 0] [1]]
                     [[1 1] [0]]]
        net (train-network (make-ml-perceptron [2 3 2 1] :tanh)
                           xor-dataset)]
    (is (< (Math/abs (- 1 (predict net [1 0])))
           0.01))
    (is (< (Math/abs (- 1 (predict net [0 1])))
           0.01))
    (is (< (Math/abs (- 0 (predict net [1 1])))
           0.01))
    (is (< (Math/abs (- 0 (predict net [0 0])))
           0.01))))
