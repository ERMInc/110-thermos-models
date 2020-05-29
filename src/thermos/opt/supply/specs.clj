(ns thermos.opt.supply.specs
  "Specs for supply problem definitions"
  (:require [spec-tools.data-spec :as ds]
            [clojure.spec.alpha :as s]))

(def cost-spec (ds/spec {:fixed number? :per-kwh number? :per-kwp number?}))

(def ^:private not-nil? (complement nil?))

(def supply-problem
  (let [time-series [number?]]
    (ds/spec
     ::supply-problem
     {:curtailment-cost number?
      :can-dump-heat    boolean?

      :discount-rate     number?
      :accounting-period pos-int?

      :co2-price  number?
      :nox-price  number?
      :pm25-price number?
      
      :profile
      {not-nil? ;; day name
       {:frequency   pos-int?
        :divisions   pos-int?
        :heat-demand time-series
        :grid-offer  time-series ;; electricity offer
        :fuel
        {not-nil?
         {:price time-series
          :co2   time-series
          :pm25  time-series
          :nox   time-series}}}}

      :plant-options
      {not-nil?
       {:capital-cost     cost-spec
        :operating-cost   cost-spec
        :lifetime         pos-int?
        :fuel             not-nil?
        :chp              boolean?
        :capacity-kwp     number?
        :power-efficiency (ds/maybe number?)
        :heat-efficiency  number?
        :substation       (ds/maybe any?)
        }}

      :storage-options
      {not-nil?
       {:capital-cost cost-spec
        :efficiency   number?
        :lifetime     pos-int?
        :capacity-kwh number? ;; storage capacity
        :capacity-kwp number? ;; maximum flow rate
        }
       }

      :substations
      {not-nil?
       {:headroom-kwp number? :alpha number?}
       }})))



