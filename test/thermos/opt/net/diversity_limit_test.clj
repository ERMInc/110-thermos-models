(ns thermos.opt.net.diversity-limit-test
  "Tests to make sure the diversity limit rule works.

  Want to make sure that applying diversity to a pipe never reduces its capacity below the capacity of any pipe downstream of it.
  "
  (:require  [clojure.test :as t]
             [thermos.opt.net.core]))

(def problem
  {:pipe-losses {:kwp [0], :w%m [0]},
   :vertices
   [{:id "A", :demand {:kwh 0, :kwp 100, :count 1, :required true}}
    {:id "B", :demand {:kwh 0, :kwp 1, :count 1, :required true}}
    {:id "S", :supply {:capacity-kw 1000000}}],
   :edges
   [{:id "1",
     :length 100,
     :cost%kwm 1, :cost%m 1
     :i "A", :j "J",
     }
    {:id "2",
     :length 100,
     :cost%kwm 1, :cost%m 0
     :i "B", :j "J",
     }
    {:id "3",
     :length 100,
     :cost%kwm 1, :cost%m 0
     :i "J", :j "S",
     }]})

(t/deftest diversity-limit
  (let [solution (thermos.opt.net.core/run-model problem)
        edges    (thermos.opt.net.core/assoc-by
                  (juxt :i :j)
                  (:edges solution))
        ]
    (t/is (> (:diversity (edges ["J" "S"])) 0.99))
    (t/is (>= (:capacity-kw (edges ["J" "S"])) 100))))
