(ns assembler-clj.core-test
  (:require [clojure.test :refer :all]
            [assembler-clj.core :refer :all]))

(deftest test-assembler
  (testing "Add.asm"
    (is (= 0 1))))
