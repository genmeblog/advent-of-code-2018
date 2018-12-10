(ns advent-of-code-2018.day09
  (:require [clojure.java.io :as io])
  (:import [java.util LinkedList]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def game-data (mapv read-string (rest (re-find #"(\d+) player.+ (\d+) points" (slurp (io/resource "day09.txt"))))))

(defn insert-at [^long pos ^LinkedList lst el]
  (let [npos (inc (int (mod (inc pos) ^int (count lst))))
        npos+ (inc npos)]
    (if (> npos+ (.size lst))
      (.add lst el)
      (.add lst npos el))
    [npos lst]))

(defn remove-at-7 [^long pos ^LinkedList lst]
  (let [cnt (count lst)
        ^long npos (mod (- (+ pos cnt) 7) cnt)
        v (.remove lst npos)]
    [v npos lst]))

(defn marble-game
  [^long players ^long max-marble]
  (fn [^long id [^long curr-pos lst] points]
    (if (> id max-marble)
      (apply max (vals points))
      (if (zero? ^long (mod id 23))
        (let [[pts pos lst] (remove-at-7 curr-pos lst)]
          (recur (inc id) [pos lst] (update points (mod (dec id) players) + id pts)))
        (recur (inc id) (insert-at curr-pos lst id) points)))))

(time {:max-score ((marble-game (first game-data) (second game-data)) 1 [0 (LinkedList. [0])]
                   (into {} (map vector (range (first game-data)) (repeat 0))))})
;; => {:max-score 422748}

(comment (marble-game 30 5807) 1 [0 (LinkedList. [0])] (into {} (map vector (range 30) (repeat 0))))


