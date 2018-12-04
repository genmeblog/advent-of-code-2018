(ns advent-of-code-2018.day04
  (:require [clojure.java.io :as io]
            [clojure.instant :as i]))

(defn log-parser
  "Parse input"
  [line]
  (let [[ydm h m op] (rest (re-find #"\[(\d+-\d+-\d+)\s(\d+):(\d+)\].*(#\d+|wakes|falls)" line))]
    {:date (i/read-instant-date (str ydm "T" h ":" m))
     :minute (Integer/parseInt m)
     :event-code (when (= \# (first op))
                   (read-string (subs op 1)))}))

(defn pack-events
  "Reorganize input"
  [[id acc] {:keys [event-code minute]}]
  (if (nil? event-code)
    [id (update acc id conj minute)]
    [event-code acc]))

;; read whole log into a map
(def log
  (->> (io/resource "day04.txt")
       (io/reader)
       (line-seq)
       (map log-parser)
       (sort-by :date)
       (reduce pack-events [-1 {}])
       (second)))

(def time-stats
  (for [[k v] log
        :let [minutes (->> (reverse v)
                           (partition 2)
                           (mapcat #(apply range %)))
              [minute how-many] (->> minutes
                                     (frequencies)
                                     (sort-by val >)
                                     (first))]]
    [k how-many minute (count minutes)]))

(defn id-with-selector
  "Sort and calculate id using selected value from stats"
  [selector]
  (let [[id _ m] (first (sort-by selector > time-stats))]
    (* id m)))

{:strategy-1 (id-with-selector last)
 :strategy-2 (id-with-selector second)}
;; => {:strategy-1 20859, :strategy-2 76576}
