(ns advent-of-code-2018.day07
  (:require [clojure.java.io :as io]
            [clojure.set :refer :all]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def coords (->> "day07.txt"
                 (io/resource)
                 (io/reader)
                 (line-seq)
                 (map #(re-find #"Step\s([A-Z]).*step\s([A-Z])" %))
                 (map rest)
                 (mapv #(let [[s1 s2] %]
                          [(first s1) (first s2)]))
                 (delay)))

(defn edges->map [f1 f2] (into {} (map (fn [[k v]]
                                         [k (set (map f2 v))]) (group-by f1 @coords))))



(defn remove-ends
  [start starts ends]
  (reduce #(update %1 %2 disj start) ends (starts start)))

(defn empty-ends
  [ends]
  (->> ends
       (filter (comp empty? second))
       (map first)))

(defn process-ends
  [curr-string candidates starts ends]
  (let [next-char (first candidates)
        new-ends-pre (remove-ends next-char starts ends)
        new-candidates (empty-ends new-ends-pre)
        curr-candidates (apply conj (disj candidates next-char) new-candidates)]
    (if (seq curr-candidates)
      (recur (str curr-string next-char) curr-candidates starts (apply dissoc new-ends-pre new-candidates))
      (str curr-string next-char))))

(defn find-order []
  (let [all (set (flatten @coords))
        starts (edges->map first second)
        ends (edges->map second first)
        starting (apply sorted-set (difference all (set (keys ends))))]
    (process-ends "" starting starts ends)))

(time {:order (find-order)})
;; => {:order "OVXCKZBDEHINPFSTJLUYRWGAMQ"}
