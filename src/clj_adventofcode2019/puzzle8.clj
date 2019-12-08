(ns clj-adventofcode2019.puzzle8
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-image
  []
  (slurp "resources/puzzle8.txt"))

(defn image-to-layers
  [image]
  (partition 6 (partition 25 image)))

(defn count-zeroes
  [layer]
  (count (filter #(= \0 %) layer)))

(defn count-chars
  [layer c]
  (count (filter #(= c %) layer)))
  
(defn run-pt1
  []
  (let [least-zeroes-layer (flatten (first
                                     (first
                                      (reduce
                                       #(if (< (second (first %2)) (second (first %1)))
                                          %2
                                          %1)
                                       {nil 10000000}
                                       (let [image-layers (image-to-layers (load-image))]
                                         (map
                                          #(let [zeroes (-> %
                                                            (flatten)
                                                            (count-zeroes))]
                                             { % zeroes })
                                          image-layers)
                                         )))))
        ones (count-chars least-zeroes-layer \1)
        twos (count-chars least-zeroes-layer \2)]
    (* ones twos)))
    
(defn run-pt2
  []
  (let [image-layers (image-to-layers (load-image))
        flattened-by-layer (map flatten image-layers)
        layer-length (count (first flattened-by-layer))]
    (spit "resources/puzzle8_out.txt" nil)
    (dorun
     (map
      (fn [line]
        (dorun (map
                (fn [c]
                  (spit "resources/puzzle8_out.txt" c :append true))
                line))
        (spit "resources/puzzle8_out.txt" "\n" :append true))
      (partition
       25
       (reduce
        (fn [prev cur]
          (map-indexed
           #(if (= (nth prev %1) \2)
              %2
              (nth prev %1))
           cur))
        (into [] (repeat layer-length \2))
        flattened-by-layer))))))
