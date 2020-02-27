(ns thermos.opt.money)

(def ^:dynamic *discount-rate* 0.0)
(def ^:dynamic *accounting-period* 1)

(def pv-sequence
  ([xs] (pv-sequence xs *discount-rate* *accounting-period*))
  ([xs r p]
   (let [xs (take p xs)]
     (if (zero? r) (reduce + xs)
         (reduce + (map-indexed
                    (fn [i v] (safe-div v (Math/pow (+ 1 r) i))) xs))))))

(def pv-recurring
  ([x] (pv-recurring *discount-rate* *accounting-period*))
  ([x r p]
   (cond
     (zero? x) 0
     (zero? r) (* p x)
     true      (pv-sequence (repeat x) r p))))


