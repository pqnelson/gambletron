(ns gambletron.fielding
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [fielding] :as schema]))

(defn create [fielding-map]
  (insert fielding (values fielding-map)))

(defn update [fielding-map changes]
  (korma/update fielding
                (set-fields changes)
                (where {:id (:id fielding-map)})))

(defn delete [fielding-map]
  (korma/delete fielding
                (where {:id (:id fielding-map)})))

(defn find-by-id [id]
  (first
   (select fielding (where {:id id}))))

(defn find-by-player [player-id]
  (select fielding (where {:player-id player-id})))
