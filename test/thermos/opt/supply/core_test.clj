(ns thermos.opt.supply.core-test
  (:require [thermos.opt.supply.core :as sut]
            [lp.scip :as scip]
            [clojure.test :as t]))

(defn- ≅ [a b]
  (< (Math/abs (- a b)) 0.001))

(defn- solve [problem]
  (-> (sut/construct-mip problem)
      (scip/solve :time-limit 1
                  :mip-gap    0.01)
      (sut/interpret-solution problem)))

(def basic
  {:storage-options []
   :plant-options []
   :substations []
   :curtailment-cost 100000.0
   :discount-rate 0
   :accounting-period 10
   :pm25-price 0
   :nox-price 0
   :co2-price 0
   :can-dump-heat false
   })

(def gas-boiler
  {:name "Gas Boiler"
   :fuel :gas
   :chp false
   :power-efficiency nil
   :heat-efficiency 0.95
   :substation nil
   :capacity-kwp 1000.0
   :capital-cost    {:per-kwh 0 :fixed 1000 :per-kwp 5}
   :operating-cost  {:per-kwh 0 :fixed 0 :per-kwp 0}
   :lifetime 20
   }
  )

(def free-store
  {:name "Free store"
   :efficiency 1.0
   :capital-cost {:per-kwh 0 :fixed 0 :per-kwp 0}
   :capacity-kwh 100000.0
   :capacity-kwp 1000.0
   :lifetime 10})

(defn quick-profile [heat-demand prices]
  (let [divisions (count heat-demand)
        zeroes (vec (repeat divisions 0.0))]
    {:day
     {:frequency 1
      :divisions (count heat-demand)
      :heat-demand heat-demand
      :grid-offer zeroes
      :fuel
      (into {} (for [[fuel price] prices]
                 [fuel
                  {:price (if (number? price)
                            (vec (repeat divisions price))
                            (vec price))
                   :co2 zeroes
                   :nox zeroes
                   :pm25 zeroes}]))}}))

(t/deftest constructs-enough-supply
  (let [gas-price 0.01
        boiler-efficiency (:heat-efficiency gas-boiler)

        problem
        (assoc basic
               :plant-options {:gas-boiler gas-boiler}
               :profile
               (quick-profile [50 100 50 100]
                              {:gas gas-price}))
        solution (solve problem)

        plant (:plant solution)
        gas-boiler-out (:gas-boiler plant)
        ]
    

    (t/is (true? (:build gas-boiler-out)))
    (t/is (== 100 (:capacity-kw gas-boiler-out)))

    ;; the output should be 12 * 50 kWh + 12 * 100 kWh * 365 days
    ;; so fuel cost should be that / 0.95 x 0.01
    (t/is (≅ (* gas-price (/ (* 365.0 (+ (* 12 50) (* 12 100))) boiler-efficiency))
           (:annual-cost (:fuel-cost gas-boiler-out))))

    ))


(t/deftest uses-storage
  (let [problem
        (assoc basic
               :plant-options {:gas-boiler gas-boiler}
               :storage-options {:free-store (assoc free-store :capacity-kwp 50)}
               :profile
               (quick-profile
                [50 100 50 100]
                {:gas [0 1 0 1]}
                ))

        solution (solve problem)
        
        gas-boiler-out (:gas-boiler (:plant solution))
        store-out (:free-store (:storage solution))
        ]

    ;; we should be charging the store when it's free
    ;; but we can only do 50kW
    (t/is (= [100.0 50.0 100.0 50.0] (:day (:output gas-boiler-out))))
    (t/is (≅ 50 (:capacity-kw store-out)))
    ))


