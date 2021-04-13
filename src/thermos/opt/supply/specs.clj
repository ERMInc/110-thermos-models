;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

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
        :substation-load-kw
        {not-nil?    time-series}
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


(def supply-solution
  (ds/spec
   (let [cap-cost {:lifetime-cost number?
                   :present-cost  number?
                   :total-cost    number?}
         op-cost  {:annual-cost  number?
                   :present-cost number?
                   :total-cost   number?}
         profile  {not-nil? [number?]} ;; day-type => values
         emission (merge op-cost
                         {:annual-emission number?
                          :total-emission number?})
         ]
     {:plant
      {not-nil? ;; plant ID really
       {:build                 boolean?
        :capacity-kw           number?
        :output                profile
        :input                 profile
        :capital-cost          cap-cost
        :operating-cost        op-cost
        :fuel-cost             op-cost
        :output-kwh            number?
        :emissions             {:nox  emission
                                :co2  emission
                                :pm25 emission}
        (ds/opt :generation)   profile
        (ds/opt :grid-revenue) op-cost
        }
       }
      :storage
      {not-nil? ;; storage id
       {:capacity-kwh number?
        :capacity-kw number?
        :output profile
        :input profile
        :capital-cost cap-cost
        }
       }
      :curtailment profile
      }))
  )
