;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.group-test
  "Simple test for whether grouped demands travel together as they should."
  (:require  [clojure.test :as t]
             [thermos.opt.net.core :as netopt]
             [com.rpl.specter :as S]))


(let [e (fn [id i j n p a]
          {:length 100 :cost%m 1 :cost%kwm 0
           :id id :i i :j j
           :bounds
           {
            :count [[n, n], [n, n]],
            :peak  [[0,p], [0,p]],
            :mean  [[0,a],[0,a]]}})

      problem
      {:pipe-losses {:kwp [30], :w%m [1]}
       :vertices
       [{:id "a", :demand {:value 3000, :kwh 1000, :kwp 30,}}
        {:id "b", :demand {:value 0,    :kwh 1000, :kwp 30,}}
        {:id "s",
         :supply {:capacity-kw 1000000, :cost%kwh 1 :cost 1}}]
       
       :edges
       [(e "x" "a" "j" 1 35 0.1)
        (e "y" "b" "j" 1 35 0.1)
        (e "z" "j" "s" 2 65 0.2)]
       
       }
      ]
  (def problem problem))

(t/deftest group-all-in
  ;; so in the base problem, we should connect a, but not b, as b is worth nothing
  (let [sol (netopt/run-model problem)]
    (t/is (= #{"a"}
             (set (map :id (filter :connected (:vertices sol)))))))

  ;; now if we put them in the same group, we don't connect b
  (let [problem (S/setval
                 [:vertices S/ALL (S/must :demand) :group]
                 "G"
                 problem)
        sol (netopt/run-model problem)]
    (t/is (= #{}
             (set (map :id (filter :connected (:vertices sol)))))))

  ;; but now if we force connect a, we also get b even though it's a bad answer
  (let [problem (->>
                 problem
                 
                 (S/setval
                  [:vertices S/ALL (S/must :demand) :group]
                  "G")
                 
                 (S/setval [:vertices S/ALL (S/pred #(= (:id %) "a")) :demand :required]
                           true)
                 
                 )
        sol (netopt/run-model problem)]
    (t/is (= #{"a" "b"}
             (set (map :id (filter :connected (:vertices sol))))))
    (t/is (neg? (:objective sol)))))


