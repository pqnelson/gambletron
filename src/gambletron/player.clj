(ns gambletron.player
  (:require [korma.core :refer [insert select set-fields where values] :as korma]
            [gambletron.schema :refer [player rosters] :as schema]))

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
  (first
   (select player (where {:id id}))))

(defn find-by-lahman-id [lahman-id]
  (first
   (select player (where {:lahman-id lahman-id}))))

(defn find-by-team-id-year [team-id year]
  (select player (korma/join rosters (= :player.id :rosters.player_id))
          (where {:rosters.year year
                  :rosters.team-tx team-id})))

(defn full-name [player-map]
  (str (:first-name player-map)
       " "
       (:last-name player-map)))
