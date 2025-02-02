;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.specs
  (:require [spec-tools.data-spec :as ds]
            [clojure.spec.alpha :as s]))

(def network-problem
  (ds/spec
   ::network-problem

   {:vertices
    [{:id any?

      (ds/opt :demand)
      {:kwh number?
       :kwp number?

       ;; these two form a tristate; both-true is invalid
       (ds/opt :required) boolean?
       (ds/opt :off-network) boolean?

       ;; value of connecting
       (ds/opt :value) number?
       (ds/opt :value%kwp) number?
       (ds/opt :value%kwh) number?

       (ds/opt :count) pos-int?

       ;; Demands can be in a group.
       ;; A whole group must be connected together.
       ;; Group equivalence is determined by =
       ;; No demand can be in two groups, WLOG.
       (ds/opt :group) any?

       (ds/opt :insulation)
       [{:id any?
         (ds/opt :cost) number?
         (ds/opt :cost%kwh) number?
         (ds/opt :maximum) number?}]

       (ds/opt :alternatives)
       [{:id any?
         (ds/opt :cost) number?
         (ds/opt :cost%kwh) number?
         (ds/opt :cost%kwp) number?
         (ds/opt :emissions) {any? number?}}]
       }

      (ds/opt :supply)
      {:capacity-kw number?
       (ds/opt :capacity-kwh) number?
       (ds/opt :cost) number?
       (ds/opt :cost%kwh) number?
       (ds/opt :cost%kwp) number?
       (ds/opt :emissions) {any? number?}

       ;; only one supply from each exclusive group may be used.
       ;; (ds/opt :exclusive-groups) (ds/maybe #{any?})
       }
      }]
    
    (ds/opt :edges)
    [{:i any?
      :j any?
      :length number?
      (ds/opt :cost%m) number?
      (ds/opt :cost%kwm) number?
      (ds/opt :required) boolean?
      (ds/opt :max-capacity%kwp) number?
      }]
    
    (ds/opt :emissions) {any? {(ds/opt :cost) number? (ds/opt :maximum) number?}}
    (ds/opt :diversity-limit) number?
    (ds/opt :diversity-rate) number?
    (ds/opt :pipe-losses) {:kwp  [number?] :w%m [number?]}

    (ds/opt :force-insulation) boolean?
    (ds/opt :supply-limit) (ds/maybe integer?)

    (ds/opt :objective-scale) (ds/maybe number?)
    (ds/opt :objective-precision) (ds/maybe number?)
    (ds/opt :edge-cost-precision) (ds/maybe number?)
    }
   
   )
  )

