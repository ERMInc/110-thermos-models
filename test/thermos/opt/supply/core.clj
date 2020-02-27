(ns thermos.opt.supply.core
  (:require [thermos.opt.supply.core :as sut]
            [clojure.test :as t]))

(def simple-supply-problem
  {:curtailment-cost 10000.0
   :can-dump-heat false

   :discount-rate 0.03
   :accounting-period 50

   ;; Hard bit is constructing this profile
   ;; from whatever the UI is going to see
   :profile
   {}
   
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
