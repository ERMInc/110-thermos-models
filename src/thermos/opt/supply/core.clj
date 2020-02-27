(ns thermos.opt.supply.core
  "Optimisation model for heat supply problems.
  
  A heat supply problem is about finding a least-cost way to build an
  energy center which can meet demand for heat following a profile.

  The ingredients of an an energy center are

  - Plant, which directly produces heat (e.g. heat pumps, boilers)
  - Storage, which shifts heat within day

  The constraining factors are
  - Grid connections, which limit electricity in-flow and out flow
  - Plant & store sizes

  The problem is phrased as a MIP, after work by Marko Aunedi.
  "
  (:require [thermos.opt.supply.specs :refer [supply-problem]]
            [thermos.opt.money :refer [pv-recurring pv-sequence
                                       *discount-rate* *accounting-period*]]))

(defn equivalize-costs
  "Given a supply problem, put equilvalized costs into it

  For plant and storage, this adds :present-cost
  For profiles, this adds :present-price and :present-grid-offer

  For emissions, this adds :present-emissions-costs
  "
  [problem]
  (binding [*discount-rate*     (:discount-rate problem 0)
            *accounting-period* (:accounting-period problem 1)]
    (let [set-opex ;; set a vector of prices to be system lifetime PV
          (fn [prices _]
            (mapv #(pv-recurring %) prices))

          combine-costs ;; add up a capex and opex term and PV them
          (fn [capex-part opex-part lifetime]
            (pv-sequence
             (map +
                  (cycle (conj (or capex-part 0)
                               (repeat (dec lifetime) 0)))
                  (repeat (or opex-part 0)))))
          
          set-present-cost ;; set the present cost for a gadget
          (fn [capex opex lifetime _]
            ;; capex might have an associated loan?
            {:fixed   (combine-costs (:fixed capex)   (:fixed opex)   lifetime)
             :per-kwh (combine-costs (:per-kwh capex) (:per-kwh opex) lifetime)
             :per-kwp (combine-costs (:per-kwp capex) (:per-kwp opex) lifetime)})
          ]
      (->> problem

           ;; this might be hard to read if you haven't seen specter.
           ;; what's happening is a bunch of updates to problem at once.
           ;; each (s/terminal) is an update operation to the value at the
           ;; path used to get there.
           
           (s/multi-transform
            (s/multi-path
             [:profile s/MAP-VALS
              (s/multi-path
               [(s/collect-one :grid-offer)
                :present-grid-offer (s/terminal set-opex)]
               [:fuel s/MAP-VALS
                (s/collect-one :price)
                :present-price (s/terminal set-opex)])]

             [:plant-options s/MAP-VALS
              (s/collect-one :capital-cost)
              (s/collect-one :operating-cost)
              (s/collect-one :lifetime)
              :present-cost
              (s/terminal set-present-cost)]

             [:storage-options s/MAP-VALS
              (s/collect-one :capital-cost)
              (s/collect-one :operating-cost)
              (s/collect-one :lifetime)
              :present-cost
              (s/terminal set-present-cost)]))
           ))))

(defn construct-mip
  [problem]
  [supply-problem :ret any?]
  
  (let [problem (equilvalize-costs problem)

        {curtailment-cost :curtailment-cost
         can-dump-heat    :can-dump-heat
         profile          :profile
         plant-options    :plant-options
         substations      :substations
         storage-options  :storage-options} problem

        plant-types       (set (keys plant-options))
        store-types       (set (keys storage-options))

        day-lengths       (->> (for [[i day] profile] [i (count (:heat-demand day))])
                               (into {}))

        day-slice-hours   (->> (for [[i l] day-lengths] [i (/ 24.0 l)])
                               (into {}))

        day-slice-weight  (->> (for [[i day] (map-indexed vector profile)]
                                 [i (* (day-slice-hours i) (:frequency day))])
                               (into {}))
        
        time-slices       (set (for [[i day] (map-indexed vector profile)
                                     s       (range (day-lengths i))]
                                 [i s]))

        previous-time-slice ;; the half-hour before a given half-hour
        (fn [[day s]]
          [day (dec (if (zero? s) (day-lengths day) s))])

        slice-hours ;; how many hours is a given time-slice (so we can make kw into kwh)
        (fn [[day _]] (day-slice-hours day))

        slice-weighted-hours ;; how many hours does a given time-slice rep in a year
        (fn [[day _]] (day-slice-weight day))

        store-efficiency
        (fn [s] (-> s storage-options :efficiency))

        substation-max-reactive-power
        (fn [s] (let [s (substations s)] (* (:alpha s) (:headroom-kwp s))))

        substation-max-power
        (fn [s] (-> s substations :headroom-kwp))

        plant-fixed-cost
        (fn [p] (-> p plant-options :present-cost :fixed))

        plant-capacity-cost
        (fn [p] (-> p plant-options :present-cost :per-kwp))

        plant-output-cost
        (fn [p s]
          (let [;; we need to use weighted hours, because it's a cost

                duration (slice-weighted-hours s)
                
                {fuel :fuel chp :chp
                 {opex :per-kwh} :operating-cost
                 ep :power-efficiency
                 eh :heat-efficiency
                 } (get plant-options p)

                [day hh] s
                profile (get profile day)
                fuel (-> profile :fuel (get fuel))
                fuel-price ^double (nth (:present-price fuel) hh)
                grid-offer ^double (nth (:present-grid-offer profile) hh)
                other-cost (-> p plant-options :present-cost :per-kwh)
                ]
            ;; TODO emissions costs go in here
            (* duration
               (+ other-cost
                  (/ (+ fuel-price
                        opex
                        (if chp (* ep grid-offer) 0))
                     eh)))))

        store-fixed-cost
        (fn [s] (-> s storage-options :present-cost :fixed))
        
        store-capacity-cost
        (fn [s] (-> s storage-options :present-cost :per-kwh))

        plant-max-capacity
        (fn [p] (-> p plant-options :capacity-kwp))

        plant-substation
        (fn [p] (-> p plant-options :substation))

        plant-grid-per-heat
        (fn [p] (let [p (get plant-options p)]
                  (if (:chp p)
                    (/ (:power-efficiency p) (:heat-efficiency p))
                    0)))

        store-max-capacity
        (fn [s] (-> s storage-options :capacity-kwh))

        heat-demand
        (fn [[d hh]] (-> profile d :heat-demand (nth hh)))
        ]
    {:vars
     {:BUILD-PLANT    {:type :binary       :indexed-by [plant-types]}
      :PLANT-SIZE-KWP {:type :non-negative :indexed-by [plant-types]}
      :HEAT-OUTPUT-KW {:type :non-negative :indexed-by [plant-types time-slices]}
      :PLANT-COST     {:type :non-negative :indexed-by [plant-types]}

      :BUILD-STORE    {:type :binary       :indexed-by [store-types]}
      :STORE-COST     {:type :non-negative :indexed-by [store-types]}
      :STORE-SIZE-KWH {:type :non-negative :indexed-by [store-types]}
      :FLOW-IN-KW     {:type :non-negative :indexed-by [store-types time-slices]}
      :FLOW-OUT-KW    {:type :non-negative :indexed-by [store-types time-slices]}
      :CHARGE-KWH     {:type :non-negative :indexed-by [store-types time-slices]} 

      :CURT-KW        {:type :non-negative :indexed-by [time-slices]}
      }

     :minimize
     [:+
      ;; plant cost
      (for [p plant-types] [:PLANT-COST p])
      ;; storage cost
      (for [s store-types] [:STORE-COST s])
      ;; curtailment cost
      (for [t time-slices]
        [:* [:CURT-KW t] (slice-weighted-hours t) curtailment-cost])
      ]

     :subject-to
     (list
      ;; Production energy balance constraints:

      (for [t time-slices]
        [(if can-dump-heat :<= :=)
         ;; consumption less than...
         [:+ (heat-demand t) (for [s store-types] [:FLOW-IN-KW s t]) ]
         ;; supply
         [:+
          (for [p plant-types] [:HEAT-OUTPUT-KW p t])
          (for [s store-types] [:FLOW-OUT-KW s t])
          [:CURT-KW t] ;; curtailment is like expensive heat-output
          ]])
      
      ;; storage flow balance constraints
      (for [s store-types t time-slices :let [h (slice-hours t)]]
        [:=
         [:CHARGE-KWH s t]
         [:+ [:CHARGE-KWH s (previous-time-slice t)]
          [:* h (store-efficiency s) [:FLOW-IN-KW s t]]
          [:- [:* h [:FLOW-OUT-KW s t]]]]])
      
      ;; substation power balance constraints
      (for [s substations t time-slices]
        [:<=
         (- (substation-max-reactive-power s))
         [:+ (for [p plant-types
                   :when (= s (plant-substation p))]
               ;; we allow grid-per-heat to vary by time
               [:* [:HEAT-OUTPUT-KW p t] (plant-grid-per-heat p t)])]
         (substation-max-power s)])
      
      ;; constraints to transfer costs to cost variables

      (for [p plant-types]
        [:=
         [:PLANT-COST p]
         [:+
          [:* (plant-fixed-cost p) [:BUILD-PLANT p]]
          [:* (plant-capacity-cost p) [:PLANT-SIZE-KWP p]]
          (for [t time-slices]
            [:* (plant-output-cost p t) [:HEAT-OUTPUT-KW p t]])
          ]])

      (for [s store-types]
        [:>=
         [:STORE-COST s]
         [:+
          [:* (store-fixed-cost s) [:BUILD-STORE s]]
          [:* (store-capacity-cost s) [:STORE-SIZE-KWH s]]]])
      
      ;; big-M constraints to make us pay fixed costs if we generate

      (for [p plant-types]
        [:<= [:PLANT-SIZE-KWP p] [:* [:BUILD-PLANT p] (plant-max-capacity p)]])

      (for [s store-types]
        [:<= [:STORE-SIZE-KWH s] [:* [:BUILD-STORE s] (store-max-capacity s)]])

      ;; Now we need to link capacity to output / amount
      (for [p plant-types t time-slices]
        [:<= [:HEAT-OUTPUT-KW p t] [:PLANT-SIZE-KWP p]])

      (for [s store-types t time-slices]
        [:<= [:CHARGE-KWH s t] [:STORE-SIZE-KWH s]])
      
      ;; TODO emissions limits
      )})
  )


