(ns gambletron.util
  (:require [clojure.string :as string]))

(defn sum [coll]
  (if (empty? coll)
    0
    (reduce + coll)))

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

;; debugging
(defmacro dbg [& body]
  `(let [x# ~@body]
     (println (str "dbg: " (quote ~@body) "=" x#))
     x#))

;; hash-map helpers
(defn dissoc-nils [record]
  (persistent!
   (dissoc!
    (transient record)
    (for [[k v] record :when (nil? v)] k))))

(defn rename-keys [map kmap]
  (persistent!
   (reduce
    (fn [m [old new]]
      (if (contains? map old)
        (assoc! m new (old map))
        m))
    (apply dissoc! (transient map) (keys kmap))
    kmap)))

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

(defmacro defconst [const-name const-val]
  `(def
    ~(with-meta const-name
       (assoc (meta const-name) :const true))
    ~const-val))

(defn median [coll]
  (let [sorted-coll (sort coll)
        N (count coll)
        mid (quot N 2)]
    (if (even? N)
      (/ (+ (nth sorted-coll mid)
            (nth sorted-coll (dec mid)))
         2)
      (nth sorted-coll mid))))

