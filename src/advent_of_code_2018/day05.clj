(ns advent-of-code-2018.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def polymer (->> (slurp (io/resource "day05.txt"))
                  (s/trim)
                  (map int)
                  (delay)))

(defn react-step
  [[^long prev-unit coll] ^long curr-unit]
  (if prev-unit
    (if (== 32 (Math/abs (- prev-unit curr-unit)))
      [nil coll]
      [curr-unit (conj coll prev-unit)])
    [curr-unit coll]))

(defn react
  [[curr-cnt coll]]
  (let [[o re] (reduce react-step [nil []] coll)
        reacted (if o (conj re o) re)
        cnt (count reacted)]
    (if (= curr-cnt cnt)
      [cnt reacted]
      (recur [cnt reacted]))))

(def removed-unit-reactions
  (delay (pmap (fn [^long lower] (let [capital (- lower 32)
                                       npolymer (filter #(not (or (== ^int % lower)
                                                                  (== ^int % capital))) @polymer)]
                                   (first (react [-1 npolymer]))))
               (range (int \a) (inc (int \z))))))

(time {:reaction-units (first (react [-1 @polymer]))
       :shortest (apply min @removed-unit-reactions)})
;; => {:reaction-units 11540, :shortest 6918}
