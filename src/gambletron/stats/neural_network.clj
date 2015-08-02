(ns gambletron.stats.neural-network
  (:require [gambletron.util :refer [defconst] :as util]
            [gambletron.stats.util :refer [sq sum] :as stats-util])
  (:import [java.util ArrayList Vector Date]
           [org.neuroph.core NeuralNetwork]
           [org.neuroph.nnet MultiLayerPerceptron]
           [org.neuroph.core.data DataSet DataSetRow]
           [org.neuroph.core.learning LearningRule SupervisedLearning]
           [org.neuroph.nnet.learning MomentumBackpropagation ResilientPropagation]
           [org.neuroph.util TransferFunctionType]))
;; This is just a clojure-friendly wrapper around parts of Neuroph I
;; found useful. If someone wanted to fork this, and extend it to be
;; some "clj-neuroph" library, go ahead. Just some thoughts about that,
;; though:
;; 1. You might want to handle bias neurons. It'll be unpleasant to code
;;    "under the hood", but calling (make-network ... {:bias true}) would
;;    be nice.
;; 2. The learning rules are totally ad hoc at the moment. The lowest
;;    hanging fruit would be to handle them better "somehow". Preferably
;;    in a manner similar to how I handled the transfer functions, i.e.,
;;    by just passing in a keyword.
;; 3. It may be necessary to refactor how I make a network currently,
;;    and isolate the (set-<rule property>! network ,,,) functions.

(set! *warn-on-reflection* true)


(def transfer-function-lookup {:gaussian  TransferFunctionType/GAUSSIAN
                               :linear    TransferFunctionType/LINEAR
                               :log       TransferFunctionType/LOG
                               :ramp      TransferFunctionType/RAMP
                               :sgn       TransferFunctionType/SGN
                               :sigmoid   TransferFunctionType/SIGMOID
                               :sin       TransferFunctionType/SIN
                               :step      TransferFunctionType/STEP
                               :tanh      TransferFunctionType/TANH
                               :trapezoid TransferFunctionType/TRAPEZOID})

(defn- get-transfer-fn-type [transfer-fn-key]
  (or (transfer-fn-key transfer-function-lookup)
      (throw (Error. (str "Unknown transfer function type "
                          transfer-fn-key)))))

;; Turns a seq of [input target] pairs into a DataSet object
(defn- make-dataset [data]
  (let [inputs-number (count (ffirst data))
        outputs-number (count (second (first data)))
        data-set (DataSet. inputs-number outputs-number)]
    (doseq [row data
            :let [^doubles input (into-array Double/TYPE (first row))
                  ^doubles desiredOutput (into-array Double/TYPE (second row))]]
      (.addRow data-set (DataSetRow. input desiredOutput)))
    data-set))

(defn- make-learning-rule [^NeuralNetwork network resilient? momentum]
  (cond
    resilient?      (ResilientPropagation.)
    (pos? momentum) (MomentumBackpropagation.)
    :else           (.getLearningRule network)))

(defn set-learning-rule! [^NeuralNetwork network rule]
  (.setLearningRule network rule))

(defn set-batch-mode! [^NeuralNetwork network flag]
  (let [^SupervisedLearning rule (.getLearningRule network)]
    (if flag
      (.setBatchMode rule true)
      (.setBatchMode rule false))
    (set-learning-rule! network rule)))

(defn set-momentum! [^MomentumBackpropagation rule momentum]
  (.setMomentum rule momentum)
  rule)

;; Given:
;; 1. a vector of integers (specifying the number of nodes in the
;;    input layer, the first hidden layer, ..., the nth hidden layer, ...,
;;    and the output layer),
;; 2. a transfer-function keyword, and optionally:
;; (i) the momentum rate,
;; (ii) the learning rate,
;; (iii) the error tolerance to terminate learning,
;; (iv) a 'resilient?' predicate for ResilientPropagation
;; construct a multilayer perceptron neural network.
;;
;; If no momentum is given, or if it is zero, the network will use
;; backpropagation without momentum.
(defn make-ml-perceptron [layers transfer-fn-key
                          & [{:keys [momentum learning-rate tol
                                     resilient?]
                              :or {momentum 0.2
                                   learning-rate 0.25
                                   resilient? false}}]]
  (let [^TransferFunctionType transfer-fn-type (get-transfer-fn-type
                                                transfer-fn-key)
        ^ints neuronInLayers (into-array Integer/TYPE layers)
        network (MultiLayerPerceptron. transfer-fn-type neuronInLayers)
        ^SupervisedLearning rule (make-learning-rule network resilient? momentum)]
    (when-not resilient?
      (when-not (zero? momentum)
        (set-momentum! rule momentum))
      (.setLearningRate rule learning-rate)
      (.setBatchMode rule false))
    (.setMaxError rule (or tol (when resilient? 1e-5) 1e-7))
    (set-learning-rule! network rule)
    network))

(defn train-network [^MultiLayerPerceptron network data]
  (.learn network (make-dataset data))
  network)

;; Will predict the output based on the input data, if the network
;; predicts singletons then it will produce an atom result.
;; For vector outputs, it will produce vector outputs.
(defn predict [^NeuralNetwork network data]
  (let [input (into-array Double/TYPE data)]
    (.setInput network input)
    (.calculate network)
    (let [result (.getOutput network)]
      (if (= 1 (alength result))
        (aget result 0)
        (vec result)))))

(def xor-dataset [[[0 1] [1]]
                  [[0 0] [0]]
                  [[1 0] [1]]
                  [[1 1] [0]]])

(defn go! []
  (let [net (train-network (make-ml-perceptron [2 3 2 1] :tanh)
                           xor-dataset)]
    (doseq [[input _] xor-dataset]
      (println "Prediction for"
               input
               "is"
               (predict net input)))))
