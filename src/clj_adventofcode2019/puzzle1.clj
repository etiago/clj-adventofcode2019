(ns clj-adventofcode2019.puzzle1
  (:gen-class))

(defn- fuel-requirements-for-mass
  [mass]
  (as-> mass n
    (/ n 3)
    (Math/floor n)
    (- n 2)))

(defn- calculate-total-fuel
  [total-acc last-fuel]
  (if (<= last-fuel 0)
    total-acc
    (recur (+ total-acc last-fuel) (fuel-requirements-for-mass last-fuel))))

(defn run-pt1
  []
  (with-open [rdr (clojure.java.io/reader "resources/puzzle1.txt")]
    (reduce
     +
     0
     (map
      #(-> %
           (Double/parseDouble)
           (fuel-requirements-for-mass))
      (line-seq rdr)))))

(defn run-pt2
  []
  (with-open [rdr (clojure.java.io/reader "resources/puzzle1.txt")]
    (reduce
     +
     0
     (map
      #(->> %
            (Double/parseDouble)
            (fuel-requirements-for-mass)
            (calculate-total-fuel 0))
      (line-seq rdr)))))
