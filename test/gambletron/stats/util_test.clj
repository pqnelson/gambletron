(ns gambletron.stats.util-test
  (:require [clojure.test :refer :all]
            [gambletron.stats.util :refer :all]))

(deftest unit-step-test
  (is (= 0 (unit-step (Math/log 0))))
  (is (= 0 (unit-step -1)))
  (is (= 1 (unit-step 0/1)))
  (is (= 1 (unit-step 0.1)))
  (is (= 1 (unit-step -0.0)))
  (is (= 1 (unit-step 0.0))))
