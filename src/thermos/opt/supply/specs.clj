(ns thermos.opt.supply.specs
  "Specs for supply problem definitions"
  (:require [spec-tools.data-spec :as ds]
            [clojure.spec.alpha :as s]))

(def cost-spec (ds/spec {:fixed double? :per-kwh double? :per-kwp double?}))

(def supply-problem
  (ds/spec
   ::supply-problem
   {:curtailment-cost double?
    :can-dump-heat boolean?

    :discount-rate double?
    :accounting-period pos-int?
    
    :profile
    {any? ;; day name
     {:frequency pos-int?
      :heat-demand [double?]
      :grid-offer  [double?] ;; electricity offer
      :fuel
      {keyword?
       {:price [double?]
        :co2   [double?]
        :pm25  [double?]
        :nox   [double?]}}}}

    :plant-options
    {any?
     {:capital-cost cost-spec
      :operating-cost cost-spec
      :lifetime pos-int?
      :fuel any?
      :chp boolean?
      :capacity-kwp double?
      :power-efficiency (ds/maybe double?)
      :heat-efficiency double?
      :substation (ds/maybe any?)
      }}

    :storage-options
    {any?
     {:capital-cost cost-spec
      :efficiency double?
      :lifetime pos-int?
      :capacity-kwh double?}
     }

    :substations
    {any?
     {:headroom-kwp double? :alpha double?}
     }}))



