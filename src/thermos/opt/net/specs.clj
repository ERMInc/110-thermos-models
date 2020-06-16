(ns thermos.opt.net.specs
  (:require [spec-tools.data-spec :as ds]
            [clojure.spec.alpha :as s]))



(def network-problem
  (ds/spec
   ::network-problem

   {:vertices
    [{(ds/opt :demand)
      {:kwh number?
       :kwp number?

       (ds/opt :required) boolean?

       (ds/opt :value) number?
       (ds/opt :value%kwp) number?
       (ds/opt :value%kwh) number?

       (ds/opt :count) pos-int?
       
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
       (ds/opt :cost) number?
       (ds/opt :cost%kwh) number?
       (ds/opt :cost%kwp) number?
       (ds/opt :emissions) {any? number?}
       }
      }]
    
    (ds/opt :edges)
    [{:i any?
      :j any?
      :length number?
      (ds/opt :cost%m) number?
      (ds/opt :cost%kwm) number?
      (ds/opt :required) boolean?
      }]
    
    (ds/opt :emissions) {any? {(ds/opt :cost) number? (ds/opt :maximum) number?}}
    (ds/opt :diversity-limit) number?
    (ds/opt :diversity-rate) number?
    (ds/opt :pipe-losses) {:kwp  [number?] :w%m [number?]}
    }
   
   )
  )

