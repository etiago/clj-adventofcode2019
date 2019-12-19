(ns clj-adventofcode2019.puzzle10
  (:gen-class)
  (:require [clojure.set :as set]))

(defn load-map
  []
  (into #{}
        (flatten
         (map-indexed
          (fn [y-idx line]
            (remove
             nil?
             (map-indexed
              (fn [x-idx c]
                (if (= c \#)
                  {:x x-idx :y y-idx}
                  nil))
              line)))
          (clojure.string/split-lines
           (slurp "resources/puzzle10.txt"))))))

(defn group-asteroids-by-key
  [key asteroids]
  (reduce
   #(merge %1 {(get %2 key) (conj (get %1 (get %2 key)) %2)})
   {}
   asteroids))

(defn proper-divisors [n]
  (let [abs-n (Math/abs n)]
    (if (= abs-n 1)
      #{}
      (into #{} (filter #(= 0 (rem abs-n %)) (range 2 (inc abs-n)))))))

(defn get-common-proper-divisors
  [a b]
  (clojure.set/intersection (proper-divisors a) (proper-divisors b)))

(defn asteroid-plus-asteroid
  [f s]
  {:x (+ (:x f) (:x s)) :y (+ (:y f) (:y s))})

(defn first-asteroid-minus-second
  [f s]
  {:x (- (:x f) (:x s)) :y (- (:y f) (:y s))})

(defn asteroid-divided-by-factor
  [asteroid factor]
  {:x (/ (:x asteroid) factor) :y (/ (:y asteroid) factor)})

(defn visible-asteroids-for-partition
  [reference-asteroid asteroid-partition]
  (into #{}
        (map
         (fn [candidate]
           (cond
             (= (:y candidate) (:y reference-asteroid)) (if (> (:x candidate) (:x reference-asteroid)) :col-right :col-left)
             (= (:x candidate) (:x reference-asteroid)) (if (> (:y candidate) (:y reference-asteroid)) :col-down :col-top)
             :else (/ (- (:y candidate) (:y reference-asteroid)) (- (:x candidate) (:x reference-asteroid)))))
         (remove #(= reference-asteroid %) asteroid-partition))))

(defn visible-asteroids
  [reference-asteroid asteroids]
  (let [asteroids-greater-than-x (group-by #(> (get % :x) (get reference-asteroid :x)) asteroids)
        visible-angles-a (visible-asteroids-for-partition reference-asteroid (get asteroids-greater-than-x false))
        visible-angles-b (visible-asteroids-for-partition reference-asteroid (get asteroids-greater-than-x true))]
    (+ (count visible-angles-a) (count visible-angles-b))))


(defn run-pt1
  []
  (let [asteroids (load-map)]

    (last
     (sort-by
      second
      (map
       (fn [coords]
         [coords (visible-asteroids coords asteroids)])
       asteroids)))))
;))

(defn distance-between-two-asteroids
  [a b]
  (Math/sqrt (+ (Math/pow (- (:x a) (:x b)) 2)
                (Math/pow (- (:y a) (:y b)) 2))))

(defn visible-asteroids-with-distances-for-partition
  [reference-asteroid asteroid-partition translation]
  (map
   (fn [candidate]
     (let [angle-key
           (cond
             (= (:y candidate) (:y reference-asteroid)) (if (> (:x candidate) (:x reference-asteroid)) :col-right :col-left)
             (= (:x candidate) (:x reference-asteroid)) (if (> (:y candidate) (:y reference-asteroid)) :col-down :col-top)
             :else (translation (Math/atan (/ (- (:y candidate) (:y reference-asteroid)) (- (:x candidate) (:x reference-asteroid))))))]
       [angle-key {(distance-between-two-asteroids candidate reference-asteroid) candidate}])) 
   (remove #(= reference-asteroid %) asteroid-partition)))

(defn visible-asteroids-with-distances
  [reference-asteroid asteroids]
;  (count
   ;(into
                                        ;#{}
  (let [asteroids-greater-than-x (group-by #(> (get % :x) (get reference-asteroid :x)) asteroids)]
  ;; (reduce
  ;;  #(if (contains? %1 (first %2))
  ;;     (assoc %1 (first %2) (sort-by (fn [e] (first (first e))) (conj (get %1 (first %2)) (second %2))))
  ;;     (assoc %1 (first %2) [(second %2)]))
    ;;  {}
    (reduce
     #(assoc %1 (first %2) (sort-by (fn [el] (first (first el))) (conj (get %1 (first %2) []) (second %2))))
     {}
     (concat
      (visible-asteroids-with-distances-for-partition reference-asteroid (get asteroids-greater-than-x false) identity) ;#(+ % (* -1 (Math/PI))))
      (visible-asteroids-with-distances-for-partition reference-asteroid (get asteroids-greater-than-x true) #(+ % (* -1 (Math/PI)))  )))))

(defn run-pt2
  []
  (let [asteroids (load-map)
        station (last
                 (sort-by
                  second
                  (map
                   (fn [coords]
                     [coords (visible-asteroids coords asteroids)])
                   asteroids)))
        visible-asteroids (clojure.set/rename-keys (visible-asteroids-with-distances (first station) asteroids) {:col-top (/ (Math/PI) 2)
                                                                                                                 :col-right 0
                                                                                                                 :col-down (/ (Math/PI) -2)
                                                                                                                 :col-left (* -1 (Math/PI))})
        sorted-visible-asteroids (sort-by first visible-asteroids)
        first-shot-pos (.indexOf (keys sorted-visible-asteroids) (/ (Math/PI) 2))]

    ;sorted-visible-asteroids
    (reduce
     #(let [base-vector (get %1 (first %2) (second %2))]
        (if (empty? base-vector)
          %1
          (do
            (when (= (inc (get %1 :shotdown)) 200)
              (println (str "Shooting down 200 " (first base-vector))))
            (if (= 1 (count base-vector))
              (do
                
                (println (str "key " (first %2) "about to shoot last" base-vector))
                (merge %1 {(first %2) []} {:shotdown (inc (get %1 :shotdown))}))
              (do
                (println (str "key " (first %2) "about to shoot " (first base-vector)))
                (merge %1 {(first %2) (rest base-vector)} {:shotdown (inc (get %1 :shotdown))}))))))
     {:shotdown 0}
     (take-while (fn [state] (not= (:shotdown state) 200)) (drop first-shot-pos (cycle sorted-visible-asteroids))))

    ))

;;     (reduce
;;      #(get sorted-visible-asteroids % [])
;;      first-shot
    ;;      (cycle sorted-visible-asteroids))

    
    ;station
    ;(take 100 (drop (:x station) (cycle (range 0 33))))
    ;(println first-shot)
;    (take-while #(< shooting-top %) (keys sorted-visible-asteroids))

