(ns gambletron.player
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [player] :as schema]))

(defn create [player-map]
  (insert player (values player-map)))

(defn update [player-map changes]
  (korma/update player
                (set-fields changes)
                (where {:id (:id player-map)})))

(defn delete [player-map]
  (korma/delete player
                (where {:id (:id player-map)})))

(defn find-by-id [id]
  (select player (where {:id id})))

