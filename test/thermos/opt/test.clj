(ns thermos.opt.test
  (:require  [clojure.test :as t]))

(defn ≈
  ([a b]   (≈ a b 0.001))
  ([a b ε] (< (Math/abs (- a b)) ε)))
