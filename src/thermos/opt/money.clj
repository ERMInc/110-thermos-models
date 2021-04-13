;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.money)

(def ^:dynamic *discount-rate* 0.0)
(def ^:dynamic *accounting-period* 1)

(defn pv-sequence
  ([xs] (pv-sequence xs *discount-rate* *accounting-period*))
  ([xs r p]
   (let [xs (take p xs)]
     (if (zero? r) (reduce + xs)
         (reduce + (map-indexed
                    (fn [i v]
                      (if (zero? v) 0
                          (/ v (Math/pow (+ 1 r) i))))
                    xs))))))

(defn pv-recurring
  ([x] (pv-recurring x *discount-rate* *accounting-period*))
  ([x r p]
   (cond
     (zero? x) 0
     (zero? r) (* p x)
     true      (pv-sequence (repeat x) r p))))

(defn periodic-sequence
  "Generate a periodic sequence of £x every i years (and so on for other xis)"
  [x i & xis]
  (apply map +
         (cycle (conj (repeat (dec i) 0) x))
         (for [[x i] (partition-all 2 xis)]
           (cycle (conj (repeat (dec i) 0) x)))))
