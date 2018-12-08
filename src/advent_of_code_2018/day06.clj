(ns advent-of-code-2018.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def coords (delay (map #(map read-string (s/split % #",\s"))
                        (->> "day06.txt"
                             (io/resource)
                             (io/reader)
                             (line-seq)))))

(defn min-max [[^int minx ^int miny ^int maxx ^int maxy] [^int x ^int y]]
  [(min minx x) (min miny y) (max maxx x) (max maxy y)])

(def bounding-box
  (delay (let [[fx fy] (first @coords)
               [^long x1 ^long y1 ^long x2 ^long y2] (reduce min-max [fx fy fx fy] (rest @coords))]
           {:box [[x1 (inc x2)]
                  [y1 (inc y2)]]
            :check-box (fn [[^long x ^long y]]
                         (or (== x1 x) (== x2 x) (== y1 y) (== y2 y)))})))

(def dist-seq
  (delay (for [^long x (apply range (first (:box @bounding-box)))
               ^long y (apply range (second (:box @bounding-box)))
               :let [dists (map-indexed (fn [idx [^long cx ^long cy]]
                                          [(+ (Math/abs (- x cx))
                                              (Math/abs (- y cy))) idx]) @coords)]]
           [[x y] dists])))

(defn find-minimal-dist [lst]
  (let [[_ coord ^long cnt] (reduce (fn [[^long min-dist curr-coord ^long cnt :as curr] [^long dist coord]]
                                      (cond
                                        (< dist min-dist) [dist coord 0]
                                        (== dist min-dist) [dist coord (inc cnt)]
                                        :else curr)) [Integer/MAX_VALUE nil 0] lst)]
    (when (zero? cnt) coord)))



(def voronoi
  (delay (keep identity (map (fn [[[x y] lst]]
                               (when-let [id (find-minimal-dist lst)] [id x y])) @dist-seq))))

(defn maximum []
  (let [on-boundary? (:check-box @bounding-box)]
    (reduce (fn [^long curr [_ lst]]
              (if (some on-boundary? lst)
                curr
                (max curr (count lst)))) 0 (group-by first @voronoi))))

(defn region []
  (->> @dist-seq
       (filter #(< ^long (reduce (fn [^long acc curr]
                                   (+ acc ^long (curr 0))) 0 (second %)) 10000))
       (count)))

(time {:largest-area (maximum)
       :region-area (region)})
;; => {:largest-area 4186, :region-area 45509}

;; draw

(let [colors (mapv #(c/gray (* 255 ^double %)) (r/->seq r/default-rng 50))
      c (canvas 400 400)]
  (with-canvas [c c]
    (set-background c :black)
    (doseq [[id x y] @voronoi]
      (set-color c (colors id))
      (point c x y))
    (set-color c :red 200)
    (doseq [[x y] @coords]
      (ellipse c x y 3 3)))
  (show-window {:canvas c}))
