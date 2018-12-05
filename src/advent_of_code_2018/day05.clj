(ns advent-of-code-2018.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(set! *unchecked-math* :warn-on-boxed)

(def polymer (->> (slurp (io/resource "day05.txt"))
                  (s/trim)
                  (map int)
                  (delay)))

(defn react
  "Approach built after discussion on slack."
  ([poly] (react nil poly))
  ([back poly]
   (if-let [^int c (first poly)]
     (if (and (seq back) (== 32 (Math/abs (- ^int (first back) c))))
       (recur (rest back) (rest poly))
       (recur (cons c back) (rest poly)))
     back)))

(def removed-unit-reactions
  (delay (pmap (fn [^long upper ^long lower]
                 (let [npolymer (remove #(or (== ^int % lower)
                                             (== ^int % upper)) @polymer)]
                   (count (react npolymer)))) (range 97 123) (range 65 91))))

(time {:reaction-units (count (react @polymer))
       :shortest (apply min @removed-unit-reactions)})
;; => {:reaction-units 11540, :shortest 6918}
