(ns advent-of-code-2018.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (line-seq (io/reader (io/resource "day12example.txt"))))

;;(subs (first in) 15)
(def rules (into {} (map #(str/split % #"\s=>\s")) (drop 2 input)))

(defn next-stage [line]
  (let [in (str "...." line "....")]
    (apply str (map #(or (rules (subs in % (+ % 5))) ".") (range (+ 4 (count line)))))))

(count (filter #(= % \#) (apply str (take 20 (iterate next-stage (subs (first input) 15))))))

(next-stage (subs (first input) 15))


;;trim..., keep position

(first (drop 19 (iterate next-stage (subs (first input) 15))))
