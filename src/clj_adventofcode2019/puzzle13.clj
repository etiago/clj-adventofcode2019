(ns clj-adventofcode2019.puzzle13
  (:import org.jline.terminal.TerminalBuilder)
  (:gen-class)
  (:require [clojure.set :as set]
            [clj-adventofcode2019.puzzle9-channels :as puzzle9-channels]
            [clojure.core.async :refer [>! <! >!! <!! go chan buffer close! thread
                                        alts! alts!! timeout]])
  )

(defn- load-code
  []
  (apply
   merge
   (map-indexed (fn [key value]
                  {key value})
                (map #(-> %
                          (clojure.string/trim)
                          (biginteger))
                     (clojure.string/split
                                        ;(slurp "resources/puzzle_pt2_test2.txt")
                      (slurp "resources/puzzle12.txt")
                 #",")) )))

(defn plot-board
  [b]
  (when (> (count (keys b)) 0)
    (let [mapping {0 " "
                   1 "█"
                   2 "░"
                   3 "▔"
                   4 "○"}
          xs (map first (keys b))
          ys (map second (keys b))
          max-y (apply max ys)
          min-y (apply min ys)
          max-x (apply max xs)
          min-x (apply min xs)]
      (doall (for [y (range min-y max-y)
            x (range min-x (inc max-x))]
        (if (= x max-x)
          (println (get mapping (int (get b [x y] 0))))
          (print (get mapping (int (get b [x y] 0))))))))))

(defn return-pretty-board
  [b]
  (let [pb (atom "")]
    (when (> (count (keys b)) 0)
      (let [mapping {0 " "
                     1 "█"
                     2 "░"
                     3 "▔"
                     4 "○"}
            xs (map first (keys b))
            ys (map second (keys b))
            max-y (apply max ys)
            min-y (apply min ys)
            max-x (apply max xs)
            min-x (apply min xs)]
        (doall (for [y (range min-y max-y)
                     x (range min-x (inc max-x))]
                 (if (= x max-x)
                   (swap! pb (fn [prev-pb] (str prev-pb (get mapping (int (get b [x y] 0))) "\n")))
                   (swap! pb (fn [prev-pb] (str prev-pb (get mapping (int (get b [x y] 0)))))))))))
    (str @pb "\nScore: " (get b [-1 0] 0))))

(defn- drawables
  ([in-channel out-channel] (drawables in-channel
                                       out-channel
                                       {:board {} :pretty-board "" :stop false}))
  ([in-channel out-channel prev-state]
                                        ;(go (>! in-channel  (get (:board prev-state) (:cur-pos prev-state) 0)))
   (let [x (<!! out-channel)
         y (<!! out-channel)
         drawable-type (<!! out-channel)]
     ;(plot-board (:board prev-state))
     (if (or (nil? x) (nil? y) (nil? drawable-type))
       (cons (assoc prev-state :stop true) nil)
       (let [new-board (assoc (:board prev-state) [x y] drawable-type)
             new-state (assoc prev-state :board new-board :pretty-board (return-pretty-board new-board))]
         (lazy-seq                                
          (cons prev-state (drawables in-channel out-channel new-state))))))))



(def incha (chan 1))
(def outcha (chan 3))

(defn run-pt1
  []
  (let [in-channel incha
        out-channel outcha ;(chan 3)
        initial-state {:program-code (load-code)
                       :running-idx 0
                       :relative-start 0
                       :input 0 ;(get @board [0 0] 0)
                       :read-from-stdin false
                       :last-operation -1
                       :input-channel in-channel
                       :output-channel out-channel
                       ;:last-operation-channel op-channel
                       }]
    (puzzle9-channels/run-pt1 initial-state)
    (let [results (thread
                    (doall
                     (take-while #(not (get % :stop))
                                 (map #(do (println (:pretty-board %)) %) (drawables in-channel out-channel)))))
          terminal (.build (TerminalBuilder/builder))
          _ (.enterRawMode terminal)
          reader (.reader terminal)
          keypress (atom 0)
          direction (atom 0)]
      (while (not= 27 @keypress)
        (swap! keypress (fn [_] (.read reader)))
        (println @keypress)
        (cond
          (= @keypress 97) (swap! direction (fn [old-direction] -1))
          (= @keypress 100) (swap! direction (fn [old-direction] 1))
          (= @keypress 115) (swap! direction (fn [old-direction] 0))
          )
        (>!! in-channel @direction)))))
                                        ;(close! in-channel)
      ;(println (:board (last results)))
      ;(count (filter #(= 2 %) (map #(second %)  (:board (last results)))))

(defn run-pt1-modified
  []
  (let [in-channel incha
        out-channel outcha ;(chan 3)
        initial-state {:program-code (load-code)
                       :running-idx 0
                       :relative-start 0
                       :input 0 ;(get @board [0 0] 0)
                       :read-from-stdin false
                       :last-operation -1
                       :input-channel in-channel
                       :output-channel out-channel
                       ;:last-operation-channel op-channel
                       }]
    (puzzle9-channels/run-pt1 initial-state)
    (let [results (thread
                    (doall
                     (take-while #(not (get % :stop))
                                 (map #(do (println (:pretty-board %)) %) (drawables in-channel out-channel)))))
          terminal (.build (TerminalBuilder/builder))
          _ (.enterRawMode terminal)
          reader (.reader terminal)
          keypress (atom 0)
          direction (atom 0)]
      (while (not= 27 @keypress)
        (swap! keypress (fn [_] (.read reader)))
        (println @keypress)
        (cond
          (= @keypress 97) (swap! direction (fn [old-direction] -1))
          (= @keypress 100) (swap! direction (fn [old-direction] 1))
          (= @keypress 115) (swap! direction (fn [old-direction] 0))
          )
        (>!! in-channel @direction)))))
