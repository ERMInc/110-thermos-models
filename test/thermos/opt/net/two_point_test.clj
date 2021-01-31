(ns thermos.opt.net.two-point-test
  "
  For several small problems, we are going to have the following:

  - Vertex A, a demand
  - This has 1000 kWh/yr and 30kWp
  - Vertex B, a supply
  - This has a unit cost of 1, and no supply limits
  - An edge between them
  - This has a variable cost of 1/kw
    Since the required capacity is 30, if used it must cost 30 * length

    So 3000

  - It will also end up with 100 W of losses.


  So, the cost of connecting up a is:

  - 3000 for the pipe
  - 1000 for the heat
  - 876.6 for the losses

  - A total of 4876.6

  So, to bother connecting we need at least that much revenue

  - in connect-a-diverse, we look at diversity
  the diversified pipe should get 81%

  so pipe costs 3000 * 0.81 = 2430.

  total cost is then (since losses are fixed in scenario)

  2430 + 1000 + 876.6 => 4306.6

  4306/2 => 2153 per building
  "  
  (:require [thermos.opt.net.core :as sut]
            [thermos.opt.test :refer [≈]]
            [clojure.test :as t]))

(def problem
  {:pipe-losses {:kwp [30], :w%m [1]}
   :vertices
   [{:id "a",
     :demand {:value 0, :kwh 1000, :kwp 30,}}
    {:id "b",
     :supply
     {:capacity-kw 1000000, :cost%kwh 1 :cost 1}}]
   
   :edges
   [
    {:id "x",
     :i "b",
     :j "a",
     :length 100,
     :cost%m 0
     :cost%kwm 1,
     }]
   }
  )

(t/deftest alternative-a
  (let [problem  (assoc-in problem [:vertices 0 :demand :alternatives]
                           [{:id 0, :cost 0, :cost%kwh 4 }])
        

        solution (sut/run-model problem)]
    (t/is (= 1 (count (:vertices solution))))
    (t/is (= 0 (:alternative (first (:vertices solution)))))
    ))

(t/deftest connect-a-diverse
  (let [problem
        
        (assoc problem
               :vertices
               [
                {:id     "a",
                 :demand {
                          :value 2160,
                          :kwh   500,
                          :kwp   15
                          }
                 },
                {:id     "c",
                 :demand {
                          :value 2160,
                          :kwh   500,
                          :kwp   15
                          }
                 },
                {:id     "b",
                 :supply {
                          :capacity-kw 1000000,
                          :cost%kwh   1,
                          :cost        1
                          }
                 }
                ]
               :edges [
                  {:id     "x",
                   :i      "b",
                   :j      "j",
                   :length 100,

                   :cost%m   0
                   :cost%kwm 1,

                   :bounds {
                            :count [[1, 2], [1, 2]],
                            :peak  [[0,35], [0,35]],
                            :mean  [[0,0.1],[0,0.1]]
                            }
                   },
                  {:id     "x0",
                   :i      "j",
                   :j      "a",
                   :length 0,

                   :cost%m   0
                   :cost%kwm 0,

                   :bounds {
                            :count [[1, 1], [1, 1]],
                            :peak  [[0,35], [0,35]],
                            :mean  [[0,0.1],[0,0.1]]
                            }
                   },
                  {:id     "x1",
                   :i      "j",
                   :j      "c",
                   :length 0,

                   :cost%m   0
                   :cost%kwm 0,

                   :bounds {
                            :count [[1, 1], [1, 1]],
                            :peak  [[0,35], [0,35]],
                            :mean  [[0,0.1],[0,0.1]]
                            }
                   }
                  ])

        solution (sut/run-model problem)

        vertices (sut/assoc-by :id (:vertices solution))
        edges    (sut/assoc-by (juxt :i :j) (:edges solution))
        b->j     (get edges ["b" "j"])
        ]

    (t/is (true? (:connected (vertices "a"))))
    (t/is (true? (:connected (vertices "c"))))
    (t/is (≈ 1876 (int (:output-kwh (vertices "b"))) 2))

    (t/is (== 24 (int (:capacity-kw b->j))))
    (t/is (≈ 0.81 (:diversity b->j)))))



(t/deftest connect-a-ins
  (let [problem
        
        (-> problem
            (assoc-in [:vertices 0 :demand :insulation]
                      [ {:id 0, :minimum 0, :maximum 100, :cost%kwh 0.9} ])
            (assoc-in [:vertices 0 :demand :required] true)
            (update-in [:vertices 1 :supply] dissoc :cost)
            )

        solution (sut/run-model problem)
        vertices (sut/assoc-by :id (:vertices solution))
        ]
    
    (t/is (true? (:connected (vertices "a"))))
    (t/is (= 100 (int
                  (-> (vertices "a")
                      :insulation
                      (first)
                      (second)))))
    (t/is (= 1776 (int (:output-kwh (vertices "b")))))))

(t/deftest connect-a
  (let [problem
        (-> problem
            (assoc-in [:vertices 0 :demand :value] 4900)
            (update-in [:vertices 1 :supply] dissoc :cost))
        
        solution (sut/run-model problem)
        vertices (sut/assoc-by :id (:vertices solution))
        ]
    (t/is (= 1876 (int (:output-kwh (vertices "b")))))
    (t/is (true? (:connected (vertices "a"))))
    (t/is (= 1 (count (:edges solution))))))

(t/deftest connect-a-no-ins
  (let [problem
        (-> problem
            (update-in [:vertices 0 :demand]
                       assoc
                       :required true
                       :insulation [ {:id 0, :minimum 0, :maximum 100, :cost%kwh 1.1} ])
            (update-in [:vertices 1 :supply] dissoc :cost))
        solution (sut/run-model problem)
        vertices (sut/assoc-by :id (:vertices solution))
        ]
    (t/is (true? (:connected (vertices "a"))))
    (t/is (empty? (:insulation (vertices "a"))))
    (t/is (= 1876 (int (:output-kwh (vertices "b")))))))

(t/deftest no-alternative-a
  (let [problem
        (-> problem
            (assoc-in [:vertices 0 :demand :alternatives]
                      [ { :id 0, :cost 0, :cost%kwh  5 } ]))
        
        solution (sut/run-model problem)
        vertices (sut/assoc-by :id (:vertices solution))
        ]
    (t/is (true? (:connected (vertices "a"))))
    (t/is (= 2 (count (:vertices solution))))
    (t/is (= 1 (count (:edges solution))))))

(t/deftest no-connect-a
  (let [problem
        (-> problem
            (assoc-in [:vertices 0 :demand :value] 4800))
        
        solution (sut/run-model problem)
        vertices (sut/assoc-by :id (:vertices solution))
        ]
    (t/is (empty? (:vertices solution)))
    (t/is (empty? (:edges solution)))))
