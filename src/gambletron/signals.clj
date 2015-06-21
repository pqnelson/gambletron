(ns gambletron.signals
  (:require [incanter.stats :as stats]
            (gambletron [batting :as batting]
                        [team :as team]
                        [util :refer [defconst] :as util])))

(defconst runs-scored :R)
(defconst opponents-runs-scored :RA)
(defconst games-played :G)
(defconst wins :W)
(defconst hits-by-batters :H)
(defconst walks-by-batters :BB)
(defconst batters-hit-by-pitchers :HBP)
(defconst at-bats :AB)
(defconst sacrifice-flies :SF)
(defconst homeruns-by-batters :HR)
(defconst triples (keyword "3B"))

(defn batting-avg [{number-hits :H at-bats :AB}]
  (/ number-hits
     at-bats))

;; how often a batter reaches base
(defn on-base-percentage [{:keys [H BB HBP AB SF]}]
  (/ (+ H BB HBP)
     (+ AB BB HBP SF)))

;; power of a hitter
(defn slugging-percentage [batting-map]
  (+ (hits-by-batters batting-map)
     (or (:2B batting-map) 0)
     (* 2 (or (triples batting-map) 0))
     (/ (* 3 (homeruns-by-batters batting-map)) 
        (at-bats batting-map))))

(defn- runs-per-game [batting-map]
  (/ (:R batting-map)
     (or (:G batting-map) 1)))

(defn team-predict [team-map]
  (+ 0.03665640892757349
     (* 0.002679730611494407 (team/slugging-percentage team-map))
     (* 0.5534669162529049 (team/on-base-percentage team-map))
     (* -0.20352473225111112 (team/batting-avg team-map))))
;; (["Arizona Diamondbacks" "ARI"]
;;  ["Atlanta Braves" "ATL"]
;;  ["Baltimore Orioles" "BAL"]
;;  ["Boston Red Sox" "BOS"]
;;  ["Chicago Cubs" "CHA"]
;;  ["Chicago White Sox" "CHN"]
;;  ["Cincinnati Reds" "CIN"]
;;  ["Cleveland Indians" "CLE"]
;;  ["Colorado Rockies" "COL"]
;;  ["Detroit Tigers" "DET"]
;;  ["Houston Astros" "HOU"]
;;  ["Kansas City Royals" "KCA"]
;;  ["Los Angeles Angels of Anaheim" "LAA"]
;;  ["Los Angeles Dodgers" "LAN"]
;;  ["Miami Marlins" "MIA"]
;;  ["Milwaukee Brewers" "MIL"]
;;  ["Minnesota Twins" "MIN"]
;;  ["New York Yankees" "NYA"]
;;  ["New York Mets" "NYN"]
;;  ["Oakland Athletics" "OAK"]
;;  ["Philadelphia Phillies" "PHI"]
;;  ["Pittsburgh Pirates" "PIT"]
;;  ["San Diego Padres" "SDN"]
;;  ["Seattle Mariners" "SEA"]
;;  ["San Francisco Giants" "SFN"]
;;  ["St. Louis Cardinals" "SLN"]
;;  ["Tampa Bay Rays" "TBA"]
;;  ["Texas Rangers" "TEX"]
;;  ["Toronto Blue Jays" "TOR"]
;;  ["Washington Nationals" "WAS"])
