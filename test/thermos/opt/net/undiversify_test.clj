(ns thermos.opt.net.undiversify-test
  "
 This is to test that demands in buildings with count > 1
 are un- and re- diversified properly.
 The interesting bits are:
  - capacity of AJ and BJ are both 30, because this is their
    /diversified/ kwp written in the inputs
  - capacity of JS is not ~42, which is what you get if you
    diversify a peak of 60 with a count of 4, but instead
    a bit more, because we infer we are summing up an even
    bigger combined peak somewhere upstream, so we expect
    less diversification
  "
  (:require  [clojure.test :as t]
             [thermos.opt.net.core :as net]))

(defn- ≈
  ([a b]   (≈ a b 0.001))
  ([a b ε] (< (Math/abs (- a b)) ε)))


(t/deftest undiversify
  (let [problem
        {:pipe-losses {:kwp [0], "w/m" [0]},
         :vertices
         [{:id "A", :demand {:kwh 0, :kwp 30, :count 2, :required true}}
          {:id "B", :demand {:kwh 0, :kwp 30, :count 2, :required true}}
          {:id "S", :supply {:capacity-kw 1000000}}],
         :edges
         [{:id "1",
           :length 100,
           "cost/kwm" 1, "cost/m" 0
           :i "A",
           :j "J",
           :bounds
           {:count [[2 2] [2 2]],
            :peak [[0 35] [0 35]],
            :mean [[0 0.1] [0 0.1]]}}
          {:id "2",
           :length 100,
           "cost/kwm" 1, , "cost/m" 0
           :i "B",
           :j "J",
           :bounds
           {:count [[2 2] [2 2]],
            :peak [[0 35] [0 35]],
            :mean [[0 0.1] [0 0.1]]}}
          {:id "3",
           :length 100,
           "cost/kwm" 1, , "cost/m" 0
           :i "J",
           :j "S",
           :bounds
           {:count [[2 4] [2 4]],
            :peak [[0 65] [0 65]],
            :mean [[0 0.2] [0 0.2]]}}]}
        solution (net/run-model problem)
        edges (net/assoc-by (juxt :i :j) (:edges solution))

        
        ]

    (t/is (≈ 30.0 (:capacity-kw (edges ["A" "J"]))))
    (t/is (≈ 30.0 (:capacity-kw (edges ["A" "J"]))))
    (t/is (≈ 53.3 (:capacity-kw (edges ["J" "S"])) 1))

    (t/is (≈ 0.81 (:diversity (edges ["B" "J"]))))
    (t/is (≈ 0.81 (:diversity (edges ["A" "J"]))))

    (t/is (≈ 0.72 (:diversity (edges ["J" "S"]))) 0.1)
    
    ))

