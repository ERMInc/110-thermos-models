;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.test
  (:require  [clojure.test :as t]))

(defn ≈
  ([a b]   (≈ a b 0.001))
  ([a b ε] (< (Math/abs (- a b)) ε)))
