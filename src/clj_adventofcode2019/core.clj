(ns clj-adventofcode2019.core
  (:gen-class)
  (:require [clj-adventofcode2019.puzzle1 :as puzzle1]
            [clj-adventofcode2019.puzzle2 :as puzzle2]
            [clj-adventofcode2019.puzzle3 :as puzzle3]
            [clj-adventofcode2019.puzzle4 :as puzzle4]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (not= 3 (count args))
    (println "Pass in exactly three args. Puzzle number, part number and nr of times to run.")
    (let [puzzle (nth args 0)
          part (nth args 1)
          runs (Integer/valueOf (nth args 2))]
      (cond
        (and (= puzzle "1") (= part "1")) (println (puzzle1/run-pt1))
        (and (= puzzle "1") (= part "2")) (println (puzzle1/run-pt2))
        (and (= puzzle "2") (= part "1")) (println (puzzle2/run-pt1))
        (and (= puzzle "2") (= part "2")) (println (puzzle2/run-pt2))
        (and (= puzzle "3") (= part "1")) (println (puzzle3/run-pt1))
        (and (= puzzle "3") (= part "2")) (println (puzzle3/run-pt2))
        (and (= puzzle "4") (= part "1")) (println (puzzle4/run-pt1))
        (and (= puzzle "4") (= part "2")) (println (time (doall (repeatedly runs puzzle4/run-pt2))))
        :else (println "Puzzle not found")))))
