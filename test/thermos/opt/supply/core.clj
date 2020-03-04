(ns thermos.opt.supply.core
  (:require [thermos.opt.supply.core :as sut]
            [thermos.opt.supply.profiles :as profiles]
            [clojure.test :as t]
            [lp.scip :as scip]))

(def elec-prices
  (vec (take-nth 2 [9.66 10.94 11.36 10.6 10.6 10.6 10.2 10.2 10 9.6 11 10.96 13.36 11.2 13.04 14.56 14.5 13.98 14.2 13.44 13.16 12.28 12.52 11.6 12.02 11.48 12.8 12 12 11.6 11.1 12.4 24.96 27.6 26.8 27.2 26.7 25.96 12.9 13.26 12.18 11.1 10.5 9.8 9.5 9.28 9.2 9.1])))

(def profile-data
  {:day-types
   {:winter 1}

   :heat-profiles ;; class => day type => profile
   {:large {:winter [0  0  0  0  0  0  23  18  16  15  14  13  12  11  10   9   8   1   0   0   0   0   0   0 ]}
    :small {:winter [0  0  0  0  0  0   0   0   0   7  14  13  12  11  10   8   1   0   0   0   0   0   0   0 ]}}

   :fuel-prices ;; fuel => day type => profile
   {:gas {:winter (vec (repeat 24 0.04))}
    :electricity {:winter elec-prices}}

   :emissions-factors ;; fuel => day type => factor => profile ???
   {:gas {:winter {:co2 (vec (repeat 24 0.45))}}
    :electricity {:winter {:co2 (mapv #(/ % 1000.0)(take-nth 2 [310 312 323 329 325 327 328 330 332 332 349 358 364 381 378 374 367 363 358 353 349 346 344 342 339 338 339 341 343 344 343 342 337 332 327 324 319 314 310 304 296 285 272 259 247 237 229 224]))}}
    }
   
   :grid-offer ;; day type => profile
   {:winter (mapv #(- (* 0.8 %) 2.0) elec-prices)}
   }
  )

(def buildings
  [{:profile :large :kwh 8000.0 :kwp 35.0}
   {:profile :small :kwh 6000.0 :kwp 28.0}])

(def simple-supply-problem
  {:curtailment-cost 10000.0
   :can-dump-heat false

   :discount-rate 0.03
   :accounting-period 50

   ;; Next bit is constructing this profile
   ;; from whatever the UI is going to see
   :profile
   (profiles/create-input-profile
    profile-data
    buildings
    (reduce + 0 (map :kwh buildings))
    (* 0.62 (reduce + 0 (map :kwp buildings))))
   
   :plant-options
   {:chp
    {:capital-cost   {:fixed 50000.0 :per-kwp 95.0 :per-kwh 0.0}
     :operating-cost {:fixed 500.0   :per-kwp 30.0 :per-kwh 5.0}
     :lifetime 25
     :fuel :gas
     :chp true
     :capacity-kwp     10000.0 ;; 10MW
     :power-efficiency 0.65
     :heat-efficiency  0.9
     :substation 0}
    
    :heat-pump
    {:capital-cost {:fixed 60000.0 :per-kwp 100.0 :per-kwh 0.0}
     :operating-cost {:fixed 500.0   :per-kwp 20.0 :per-kwh 5.0}
     :lifetime 25
     :fuel :electricity
     :chp false
     :capacity-kwp 10000
     :heat-efficiency 3.0
     :substation 0
     }
    }

   :storage-options {}

   :substations
   {0 {:headroom-kwp 8000 :alpha 0.9}}})

(def supply-lp
  (sut/construct-mip simple-supply-problem))


(require '[lp.scip :as scip])

(def solution
  (scip/solve supply-lp
              :scipampl "/nix/store/mdd05ii27lq0j7q1lzv9bpv95p5wgayh-scipoptsuite-6.0.0/bin/scipampl"
              ))
