(ns gambletron.util
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string])
  (:import [java.util UUID]))

(defn generate-uuid ^UUID []
  (UUID/randomUUID))

(defn uuid? [obj]
  (instance? java.util.UUID obj))

(defn uuid->string ^String [uuid]
  (string/replace
   (if (uuid? uuid) (.toString uuid) uuid)
   "-" ""))

(defn string->uuid ^UUID [^String s]
  (UUID/fromString
   (str (subs s 0 8)
        "-"
        (subs s 8 12)
        "-"
        (subs s 12 16)
        "-"
        (subs s 16 20)
        "-"
        (subs s 20))))

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

(defmacro sq [x] `(* ~x ~x))
(defmacro cube [x] `(* ~x ~x ~x))

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
(defn static? [^java.lang.reflect.Field field]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers field)))

(defn get-record-field-names [^Class record]
  (->> record
       .getDeclaredFields
       (remove static?)
       (map (fn [^java.lang.reflect.Field f] (.getName f)))
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

(defn demunge [^String s]
  (clojure.lang.Compiler/demunge s))

;; TODO: consider using demunge instead?
;; key-renaming helpers
(defn snake->kabob [^String s]
  (.replaceAll s "_" "-"))

;; Consider using (munge s) instead?
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

;; From https://gist.github.com/cemerick/747395
(defn- mutable-memoize
  [f ^java.util.Map map]
  (fn [& args]
    (if-let [e (find map args)]
      (val e)
      (let [ret (apply f args)]
        (.put map args ret)
        ret))))

(defn soft-memoize
  [f]
  (let [m (java.util.concurrent.ConcurrentHashMap.)
        rq (java.lang.ref.ReferenceQueue.)
        memoized (mutable-memoize
                   #(java.lang.ref.SoftReference. (apply f %&) rq)
                   m)]
    (fn clear-fn [& args]
      ; clearCache conveniently exists to poll the queue and scrub a CHM
      ; used in Clojure's Keyword and DynamicClassLoader, so it's not going anywhere
      (clojure.lang.Util/clearCache rq m)
      (let [^java.lang.ref.SoftReference ref (apply memoized args)
            val (.get ref)]
        (if (.isEnqueued ref)
          ; reference enqueued since our clearCache call above, retry
          (apply clear-fn args)
          val)))))
