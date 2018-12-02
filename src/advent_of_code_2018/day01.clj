(ns advent-of-code-2018.day01
  (:require [clojure.java.io :as io]))

(def frequencies-diff
  (map read-string (-> (io/resource "day01.txt")
                       (io/reader)
                       (line-seq))))

(def freq-dup
  (let [freqs (reductions + 0 (cycle frequencies-diff))]
    (reduce (fn [visited freq]
              (if (visited freq)
                (reduced freq)
                (conj visited freq))) #{} freqs)))

{:final-frequency (reduce + frequencies-diff)
 :duplicated-frequency freq-dup}
;; => {:final-frequency 582, :duplicated-frequency 488}
