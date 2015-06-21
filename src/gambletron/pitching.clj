(ns gambletron.pitching
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [pitching] :as schema]))

(defn create [pitching-map]
  (insert pitching (values pitching-map)))

(defn update [pitching-map changes]
  (korma/update pitching
                (set-fields changes)
                (where {:id (:id pitching-map)})))

(defn delete [pitching-map]
  (korma/delete pitching
                (where {:id (:id pitching-map)})))

(defn find-by-id [id]
  (first
   (select pitching (where {:id id}))))

(defn find-by-player [player-id]
  (select pitching (where {:player-id player-id})))

(defn find-by-team-id [team-id]
  (select pitching (where {:team-id team-id})))

(defn find-by-team [team]
  (find-by-team-id (:id team)))
