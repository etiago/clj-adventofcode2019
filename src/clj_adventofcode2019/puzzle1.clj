(ns clj-adventofcode2019.puzzle1
  (:gen-class))

;; Consciously eagerly loading file into memory.
;; Lazy sequence from a file makes the code slightly more ugly.
(def puzzle-input
  (->> (slurp "resources/puzzle1.txt")
       (clojure.string/split-lines)
       (map #(Double/parseDouble %))))

(defn- fuel-requirements-for-mass
  [mass]
  (-> mass
    (/ 3)
    (Math/floor)
    (- 2)))

(defn- calculate-total-fuel
  [total-acc last-fuel]
  (if (<= last-fuel 0)
    total-acc
    (recur
     (+ total-acc last-fuel)
     (fuel-requirements-for-mass last-fuel))))

(defn- total-fuel-requirements-for-mass
  [mass]
  (->> mass
       (fuel-requirements-for-mass)
       (calculate-total-fuel 0)))

(defn- map-reduce-fuel-requirements-for-mass
  [input]
  (->> input
      (map fuel-requirements-for-mass)
      (reduce +)))

(defn- map-reduce-total-fuel-requirements-for-mass
  [input]
  (->> input
      (map total-fuel-requirements-for-mass)
      (reduce +)))

(defn run-pt1
  []
  (map-reduce-fuel-requirements-for-mass puzzle-input))

(defn run-pt2
  []
  (map-reduce-total-fuel-requirements-for-mass puzzle-input))
