(ns gambletron.util
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]))

(defmacro defconst [const-name const-val]
  `(def
    ~(with-meta const-name
       (assoc (meta const-name) :const true))
    ~const-val))

;;; debugging
(defmacro dbg [& body]
  `(let [x# ~@body]
     (println "dbg:" (str (quote ~@body)) "=" x#)
     x#))

;;; random math helpers
(defn sum [coll]
  (if (empty? coll)
    0
    (reduce + coll)))

(defn mean [coll]
  (when (seq coll)
    (/ (reduce + coll) (count coll))))

(defn median [coll]
  (let [sorted-coll (sort coll)
        N (count coll)
        mid (quot N 2)]
    (if (even? N)
      (/ (+ (nth sorted-coll mid)
            (nth sorted-coll (dec mid)))
         2)
      (nth sorted-coll mid))))

(defn sq [x] (* x x))

;; See Knuth, TAOCP vol 2, 3d ed, pg 232
(defn- online-variance [coll]
  (when (and (seq coll) (seq (rest coll)))
    (loop [[x & tail] coll
           n 1
           mu 0.0
           m2 0.0]
      (if-not x
        (/ (dbg m2) (dec n))
        (let [delta (- x mu)]
          (recur tail
                 (inc n)
                 (+ mu (/ delta n))
                 (+ m2 (* delta (- x mu)))))))))

(defn compensated-var [coll]
  (when (seq coll)
    (let [mu (mean coll)]
      (/ (- (sum (map #(sq (- % mu)) coll))
            (/ (sq (sum (map #(- % mu) coll)))
               (count coll)))
         (dec (count coll))))))

(defn variance [coll]
  (when (seq coll)
    (let [mu (mean coll)]
       (double
        (/ (sum (map (fn [x] (sq (- x mu))) coll))
           (dec (count coll)))))))

(defn standard-dev [coll]
  (when (seq coll)
    (Math/sqrt (variance coll))))

;; record helpers
(defn static? [field]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers field)))

(defn get-record-field-names [record]
  (->> record
       .getDeclaredFields
       (remove static?)
       (map #(.getName %))
       (remove #{"__meta" "__extmap"})))

(defmacro empty-record [record]
  (let [klass (Class/forName (name record))
        field-count (count (get-record-field-names klass))]
    `(new ~klass ~@(repeat field-count nil))))


;;; hash-map helpers
(defn dissoc-nils [record]
  (persistent!
   (dissoc!
    (transient record)
    (for [[k v] record :when (nil? v)] k))))

(defn invert-keys [map]
  (zipmap (vals map) (keys map)))

(defn rename-keys [map kmap]
  (persistent!
   (reduce
    (fn [m [old new]]
      (if (contains? map old)
        (assoc! m new (old map))
        m))
    (apply dissoc! (transient map) (keys kmap))
    kmap)))

(defn map-vals [f m]
  (zipmap (keys m) (map f (vals m))))

;; key-renaming helpers
(defn snake->kabob [^String s]
  (.replaceAll s "_" "-"))

(defn kabob->snake [^String s]
  (.replaceAll s "-" "_"))

(defn snake-case-keyword [k]
  (keyword (kabob->snake (name k))))

(defn dash-keys [map]
  (into {}
        (for [[k v] map]
          [(keyword (snake->kabob (name k))) v])))

(defn underscore-keys [map]
  (into {}
        (for [[k v] map]
          [(keyword (kabob->snake (name k))) v])))

