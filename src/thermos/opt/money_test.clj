;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.money-test
  (:require [thermos.opt.money :as sut]
            [thermos.opt.test :refer [≈]]
            [clojure.test :as t]))

(t/deftest bindings
  (binding [sut/*discount-rate*     0
            sut/*accounting-period* 10]
    (t/is (= 3 (sut/pv-sequence [1 1 1])))
    (t/is (== 20 (sut/pv-sequence (repeat 2))))

    (t/is (== 10 (sut/pv-recurring 1)))

    (t/is (≈ (/ 2 1.1 1.1 1.1) (sut/pv-sequence [0 0 0 2]
                                                0.1 10)))))

(t/deftest discount
  (binding [sut/*discount-rate* 0.1
            sut/*accounting-period* 10]
    (t/is (== 1 (sut/pv-sequence [1])))
    (t/is (== (/ 1 1.1) (sut/pv-sequence [0 1])))
    (t/is (== (/ 1 1.1 1.1) (sut/pv-sequence [0 0 1])))
    (t/is (≈ (/ 2 1.1 1.1 1.1) (sut/pv-sequence [0 0 0 2])))))


(t/deftest periodic-sequence
  (t/is (= [30 0 0 0 30 0 0 0]
           (vec (take 8 (sut/periodic-sequence 30 4)))))

  (t/is (= [45 0 0 15 30 0 15 0]
           (vec (take 8 (sut/periodic-sequence 30 4 15 3))))))

