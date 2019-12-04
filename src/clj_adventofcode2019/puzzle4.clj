(ns clj-adventofcode2019.puzzle4
  (:gen-class))

(defn has-adjacent-repeated?
  [p]
  (some
   true?
   (map
    #(= (nth p %) (nth p (inc %)))
    (range 0 (dec (count p))))))

(defn always-increasing?
  [p]
  (every?
   true?
   (map
    #(<= (Integer/valueOf (subs p % (inc %))) (Integer/valueOf (subs p (inc %) (+ 2 %))))
    (range 0 (dec (count p))))))

(defn nearest-always-increasing
  [p]
  (Integer/valueOf
   (clojure.string/join
    (reduce 
     #(if (== (count p) (count %1))
        %1
        (if (<= (Integer/valueOf (subs p (dec %2) %2))
                (Integer/valueOf (subs p %2 (inc %2))))
          (conj %1 (subs p %2 (inc %2)))
         (into [] (concat %1 (repeat (- (count p)  %2) (nth %1 (dec (count %1))))))))
    [(subs p 0 1)]
    (range 1 (count p))))))

(defn run-pt1
  []
  (let [potential-passwords (map #(String/valueOf %) (range 123257 647015))]
    (count (filter #(and (has-adjacent-repeated? %) (always-increasing? %)) potential-passwords))))
    
(defn has-adjacent-repeated-no-more-than-two?
  [p]
  (let [f (frequencies p)]
    (some
     true?
     (map
      #(and (= (nth p %) (nth p (inc %))) (= 2 (get f (nth p %))))
      (range 0 (dec (count p)))))))

(defn hacky-has-adjacent-repeated-no-more-than-two?
  [p]
  (or (and (= (nth p 0) (nth p 1))
           (not= (nth p 1) (nth p 2)))
      (and (not= (nth p 0) (nth p 1))
           (= (nth p 1) (nth p 2))
           (not= (nth p 2) (nth p 3)))
      (and (not= (nth p 1) (nth p 2))
           (= (nth p 2) (nth p 3))
           (not= (nth p 3) (nth p 4)))
      (and (not= (nth p 2) (nth p 3))
           (= (nth p 3) (nth p 4))
           (not= (nth p 4) (nth p 5)))
      (and (not= (nth p 3) (nth p 4))
           (= (nth p 4) (nth p 5)))))

(defn growing-numbers 
  [start]
  (let [str-start (String/valueOf start)]
    (if (always-increasing? str-start)
      (lazy-seq (cons start (growing-numbers (inc start))))
      (lazy-seq (growing-numbers (nearest-always-increasing str-start))))))

(defn run-pt2
  []
  (let [potential-passwords
        (map
         #(String/valueOf %)
         ;(take-while #(<= % 805915) (growing-numbers 347312)))]
         (take-while #(<= % 647015) (growing-numbers 123257)))]
    (count
     (filter
      #(hacky-has-adjacent-repeated-no-more-than-two? %)
      potential-passwords))))
