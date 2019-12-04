(ns clj-adventofcode2019.puzzle1-test
  (:gen-class)
  (:require
        [clojure.test :refer :all]
        [clj-adventofcode2019.puzzle1 :refer :all]))

(deftest fuel-requirements-for-mass'-test
  (testing
      "Happy Path. Requirements for 100 equals 31"
    (is (= 31.0 (#'clj-adventofcode2019.puzzle1/fuel-requirements-for-mass 100)))))

(deftest fuel-requirements-for-mass'-test-zero-mass-negative-fuel
  (testing
      "Zero mass requires minus two fuel"
    (is (= -2.0 (#'clj-adventofcode2019.puzzle1/fuel-requirements-for-mass 0)))))



