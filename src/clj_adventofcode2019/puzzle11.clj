(ns clj-adventofcode2019.puzzle11
  (:gen-class)
  (:require [clojure.set :as set]
            [clj-adventofcode2019.puzzle9 :as puzzle9]
            [clj-adventofcode2019.puzzle9-channels :as puzzle9-channels]
            [clojure.core.async :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! timeout]]))

(defn- load-code
  []
  (apply merge (map-indexed (fn [key value]
         {key value})
         (map #(-> %
                   (clojure.string/trim)
                                        (biginteger)
                   ;(Integer/parseInt)
                   )
                     ;(Integer/parseInt))
                (clojure.string/split
                 ;(slurp "resources/puzzle_pt2_test2.txt")
                 (slurp "resources/puzzle11.txt")
                 #",")) )))

;(def state (atom {}))
;; (def board (atom {[0 0] 1}))
;; (def cur-pos (atom [0 0]))
;; (def direction-mask (atom [0 1]))
(def turn-left
  {[0 1] [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]
   [1 0] [0 1]})
(def turn-right
  {[0 1] [1 0]
   [1 0] [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]})
;; (def stop (atom false))

;; (defn output-for-input
;;   [input in-channel out-channel]
;;   (>!! in-channel input)
;;   (<!! out-channel))
;;   ;(swap! state (fn [old-state] (assoc old-state :input input)))
;; ;  (swap! state (fn [old-state] (first (puzzle9/run-pt1 old-state))))
;; ;  (get @state :output))

(defn plot-board
  [b]
  (let [mapping {0 "□"
                 1 "■"}
        xs (map first (keys b))
        ys (map second (keys b))
        max-y (apply max ys)
        min-y (apply min ys)
        max-x (apply max xs)
        min-x (apply min xs)]
    (for [y (range max-y (dec min-y) -1)
          x (range min-x max-x)]
      (if (= x (dec max-x))
        (println (get mapping (int (get b [x y] 0))))
        (print (get mapping (int (get b [x y] 0))))))))

(defonce op-channel (chan 100000))
(defn- move
  ([in-channel out-channel] (move in-channel
                                  out-channel
                                  {:cur-pos [0 0]
                                   :color-to-paint 0
                                   :turn 0
                                   :board {[0 0] 1}
                                   :direction-mask [0 1]
                                   :stop false} ))
  ([in-channel out-channel prev-state]
   (go (>! in-channel  (get (:board prev-state) (:cur-pos prev-state) 0)))
   (let [color-to-paint (<!! out-channel)
         turn (<!! out-channel)]
     (if (or (nil? color-to-paint) (nil? turn))
       (cons (assoc prev-state :stop true) nil)
       (let [new-direction-mask (if (= turn 0) (get turn-left (:direction-mask prev-state)) (get turn-right (:direction-mask prev-state)))
             new-state (assoc prev-state
                              :board (assoc (:board prev-state) (:cur-pos prev-state) color-to-paint)
                              :direction-mask new-direction-mask
                              :cur-pos [(+ (first (:cur-pos prev-state)) (first new-direction-mask)) (+ (second (:cur-pos prev-state)) (second new-direction-mask))])]
         (lazy-seq                                
          (cons prev-state (move in-channel out-channel new-state))))))))

(defn- get-results
  []
  (let [in-channel (chan 1)
        out-channel (chan 2)
        initial-state {:program-code (load-code)
                       :running-idx 0
                       :relative-start 0
                       :input 0 ;(get @board [0 0] 0)
                       :read-from-stdin false
                       :last-operation -1
                       :input-channel in-channel
                       :output-channel out-channel
                       :last-operation-channel op-channel}]
    (puzzle9-channels/run-pt1 initial-state)
    (let [results (doall (take-while #(not (get % :stop)) (move in-channel out-channel)))]
      (close! in-channel)
      results)))

    ;;(while (not @stop) ;(not= (<!! op-channel) 99) ;(not= (get @state :last-operation) 99)
    ;;  (let [_ (go (>! in-channel (get @board @cur-pos 0)))
    ;;        color-to-paint (<!! out-channel)
    ;;        turn (<!! out-channel)]
    ;;    (if (or (nil? color-to-paint) (nil? turn))
    ;;      (swap! stop (fn [_] true))
    ;;                                    ;(println @state)
    ;;                                    ;(println @board)
    ;;                                    ;(println color-to-paint)
    ;;      (do
    ;;        (swap! board (fn [old-board] (assoc old-board @cur-pos color-to-paint)))
    ;;        (swap! direction-mask (fn [old-mask] (if (= turn 0) (get turn-left old-mask) (get turn-right old-mask))))                                               
    ;;        (swap! cur-pos (fn [old-pos] [(+ (first old-pos) (first @direction-mask)) (+ (second old-pos) (second @direction-mask))]))))))
;      )))

(defn run-pt1
  []
  (keys (:board (last (get-results)))))

(defn run-pt2
  []
  (plot-board (:board (last (get-results)))))

