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

  The main useful functions are `construct-mip` and `interpret-solution`, which see.

  - `construct-mip` produces a MIP suitable for evaluation by the `lp.core` functoins
  - `interpret-solution` extracts useful information from a solved MIP
  "
  (:require [thermos.opt.supply.specs :refer [supply-problem]]
            [thermos.opt.money :refer [pv-recurring pv-sequence periodic-sequence
                                       *discount-rate* *accounting-period*]]
            [com.rpl.specter :as s]
            [clojure.spec.alpha :as spec]
            [clojure.tools.logging :as log]
            [clojure.test :as test]))

(defn equivalize-costs
  "Given a supply problem, put equilvalized costs into it

  For plant and storage, this adds :present-cost
  For profiles, this adds :present-price and :present-grid-offer

  For emissions, this adds :present-emissions-costs
  "
  [problem]
  (binding [*discount-rate*     (:discount-rate problem 0)
            *accounting-period* (:accounting-period problem 1)]
    (let [{:keys [co2-price nox-price pm25-price]} problem

          set-fuel-price
          (fn [price co2 nox pm25 _]
            (mapv pv-recurring
                  (map +
                       price
                       (map #(* % co2-price) co2)
                       (map #(* % nox-price) nox)
                       (map #(* % pm25-price) pm25))))
          
          set-opex ;; set a vector of prices to be system lifetime PV
          (fn [prices _]
            (mapv pv-recurring prices))

          combine-costs ;; add up a capex and opex term and PV them
          (fn [capex-part opex-part lifetime]
            (pv-sequence (periodic-sequence
                          (or capex-part 0) lifetime
                          (or opex-part 0)  1)))
          
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
                (s/collect-one :co2)
                (s/collect-one :nox)
                (s/collect-one :pm25)
                :present-price (s/terminal set-fuel-price)
                ])]

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

(defn- valid? [x y]
  (let [is-valid (spec/valid? x y)]
    (when-not is-valid
      (log/error (spec/explain-str x y)))
    is-valid))

(defn construct-mip
  [problem]
  {:pre [(valid? supply-problem problem)]}
  
  (let [problem (equivalize-costs problem)

        {curtailment-cost :curtailment-cost
         can-dump-heat    :can-dump-heat
         profile          :profile
         plant-options    :plant-options
         substations      :substations
         storage-options  :storage-options} problem

        substation-ids    (set (keys substations))
        plant-types       (set (keys plant-options))
        store-types       (set (keys storage-options))

        day-lengths       (->> (for [[i day] profile] [i (:divisions day)])
                               (into {}))

        day-slice-hours   (->> (for [[i l] day-lengths] [i (/ 24.0 l)])
                               (into {}))

        day-slice-weight  (->> (for [[i day] profile]
                                      [i (* (day-slice-hours i) (:frequency day))])
                               (into {}))
        
        time-slices       (set (for [[i day] profile
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

        substation-load
        (fn [s [day hh]]
          (-> profile (get day) :substation-load-kw (get s) (get hh 0)))
        
        substation-max-reactive-power
        (fn [s t]
          (let [load-kw (substation-load s t)
                s (substations s)]
            (+ load-kw
               (* (:alpha s 1.0) (:headroom-kwp s 0)))))

        substation-max-power
        (fn [s t]
          (- (-> s substations (:headroom-kwp 0))
             (substation-load s t)))

        plant-fixed-cost
        (fn [p] (-> p plant-options :present-cost :fixed))

        plant-capacity-cost
        (fn [p] (-> p plant-options :present-cost :per-kwp))

        plant-output-cost
        (fn [p s]
          (let [;; we need to use weighted hours, because it's a cost
                duration (slice-weighted-hours s)
                
                {fuel :fuel chp :chp
                 ep :power-efficiency
                 eh :heat-efficiency
                 } (get plant-options p)

                [day hh] s
                profile (get profile day)

                fuel (-> profile :fuel (get fuel))
                fuel-price ^double (get (:present-price fuel) hh)
                grid-offer ^double (get (:present-grid-offer profile) hh)
                other-cost (-> p plant-options :present-cost :per-kwh)
                ]
            (* duration
               (+ other-cost
                  ;; emissions costs have been rolled into
                  ;; present-price of fuel
                  (/ (+ fuel-price (if chp (* (or ep 0) grid-offer) 0)) eh)))))

        store-fixed-cost
        (fn [s] (-> s storage-options :present-cost :fixed))
        
        store-capacity-cost
        (fn [s] (-> s storage-options :present-cost :per-kwh))

        store-flow-cost
        (fn [s] (-> s storage-options :present-cost :per-kwp))

        plant-max-capacity
        (fn [p] (-> p plant-options :capacity-kwp))

        plant-substation
        (fn [p] (-> p plant-options :substation))

        plant-grid-per-heat
        (fn [p t] (let [p (get plant-options p)]
                    (if (:chp p)
                      (/ (:power-efficiency p 0) (:heat-efficiency p))
                      (/ (:heat-efficiency p)))))

        store-max-capacity
        (fn [s] (-> s storage-options :capacity-kwh))

        store-max-flow
        (fn [s] (-> s storage-options :capacity-kwp))

        heat-demand
        (fn [[d hh]] (-> profile (get d) (get :heat-demand) (nth hh)))
        ]
    {:vars
     (cond->
         {:BUILD-PLANT    {:type :binary       :indexed-by [plant-types]}
          :PLANT-SIZE-KWP {:type :non-negative :indexed-by [plant-types]}
          :HEAT-OUTPUT-KW {:type :non-negative :indexed-by [plant-types time-slices]}
          :PLANT-COST     {:type :non-negative :indexed-by [plant-types]}
          :CURT-KW        {:type :non-negative :indexed-by [time-slices]}}

       (not-empty store-types) ;; add store vars in only if there are any
       (merge
        {:BUILD-STORE    {:type :binary       :indexed-by [store-types]}
         :STORE-COST     {:type :non-negative :indexed-by [store-types]}
         :STORE-SIZE-KWH {:type :non-negative :indexed-by [store-types]
                          :upper store-max-capacity}
         :STORE-SIZE-KWP {:type :non-negative :indexed-by [store-types]
                          :upper store-max-flow}
         
         :FLOW-IN-KW     {:type :non-negative :indexed-by [store-types time-slices]
                          :upper (fn [s _] (store-max-flow s))}
         
         :FLOW-OUT-KW    {:type :non-negative :indexed-by [store-types time-slices]
                          :upper (fn [s _] (store-max-flow s))}
         :CHARGE-KWH     {:type :non-negative :indexed-by [store-types time-slices]}}))

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
          (for [s store-types] [* (store-efficiency s) [:FLOW-OUT-KW s t]])
          [:CURT-KW t] ;; curtailment is like expensive heat-output
          ]])
      
      ;; storage flow balance constraints
      (for [s store-types t time-slices :let [h (slice-hours t)]
            :let [t* (previous-time-slice t)]]
        [:=
         [:CHARGE-KWH s t]
         [:+ [:CHARGE-KWH s t*]
          [* h [- [:FLOW-IN-KW s t*] [:FLOW-OUT-KW s t*]]]]])
      
      ;; substation power balance constraints
      (for [s substation-ids t time-slices]
        [:<=
         (- (substation-max-reactive-power s t))
         [:+ (for [p plant-types
                   :when (= s (plant-substation p))]
               ;; we allow grid-per-heat to vary by time
               [:* [:HEAT-OUTPUT-KW p t] (plant-grid-per-heat p t)])]
         (substation-max-power s t)])
      
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
          [:* (store-flow-cost s) [:STORE-SIZE-KWP s]]
          [:* (store-capacity-cost s) [:STORE-SIZE-KWH s]]]])
      
      ;; big-M constraints to make us pay fixed costs if we generate
      ;; these also prevent us building store / plant that is larger
      ;; than max capacity

      (for [p plant-types]
        [:<= [:PLANT-SIZE-KWP p] [:* [:BUILD-PLANT p] (plant-max-capacity p)]])

      (for [s store-types]
        [:<= [:STORE-SIZE-KWH s] [:* [:BUILD-STORE s] (store-max-capacity s)]])

      ;; Now we need to link capacity to output / amount
      (for [p plant-types t time-slices]
        [:<= [:HEAT-OUTPUT-KW p t] [:PLANT-SIZE-KWP p]])

      ;; Store overall size enough for stored amount
      (for [s store-types t time-slices]
        [:<= [:CHARGE-KWH s t] [:STORE-SIZE-KWH s]])

      ;; Store connection size enough for peak in/out flow
      (for [s store-types t time-slices]
        [:and
         [:>= [:STORE-SIZE-KWP s] [:FLOW-IN-KW s t]]
         [:>= [:STORE-SIZE-KWP s] [:FLOW-OUT-KW s t]]])
      )}))


(defn- plant-capital-cost
  "Calculate the capital cost of plant, for output"
  [problem plant-type kwp kwh]
  (let [{:keys [fixed per-kwh per-kwp]}
        (-> problem :plant-options (get plant-type) :capital-cost)

        lifetime (-> problem :plant-options (get plant-type) :lifetime)
        
        cost-per-lifetime
        (+ fixed
           (* kwp per-kwp)
           (* kwh per-kwh))
        ]
    {:lifetime-cost cost-per-lifetime
     ;; Assuming *accounting-period* is bound by caller
     :present-cost  (pv-sequence (periodic-sequence cost-per-lifetime lifetime))
     :total-cost    (binding [*discount-rate* 0]
                      (pv-sequence (periodic-sequence cost-per-lifetime lifetime)))
     }))

(defn- plant-operating-cost [problem plant-type kwp kwh]
  (let [{:keys [fixed per-kwh per-kwp]}
        (-> problem :plant-options (get plant-type) :operating-cost)

        problem-span (-> problem :accounting-period)
        
        annual-cost
        (+ fixed
           (* kwp per-kwp)
           (* kwh per-kwh))
        ]
    {:annual-cost annual-cost
     :present-cost (pv-recurring annual-cost)
     :total-cost (* annual-cost problem-span)}))

(defn- store-capital-cost [problem store-type kwp kwh]
  (let [{:keys [fixed per-kwh per-kwp]}
        (-> problem :storage-options (get store-type) :capital-cost)

        lifetime (-> problem :storage-options (get store-type) :lifetime)
        
        cost-per-lifetime
        (+ fixed
           (* kwp per-kwp)
           (* kwh per-kwh))
        ]
    {:lifetime-cost cost-per-lifetime
     :present-cost  (pv-sequence (periodic-sequence cost-per-lifetime lifetime))
     :total-cost    (binding [*discount-rate* 0]
                      (pv-sequence (periodic-sequence cost-per-lifetime lifetime)))
     }))

(defn- sum-over-profile [day-types quantity]
  ;; output is {day type => [divisions]} for this plant
  ;; so we want to scale up / down by day frequency & division
  ;; to total up for a year.
  {:test #(do
            (test/is
             (= 8760.0 (sum-over-profile
                        {0 {:frequency 1 :divisions 1}}
                        {0 [1]})))

            (test/is
             (= (* 1.5 8760.0)
                (sum-over-profile
                 {0 {:frequency 1 :divisions 1}
                  1 {:frequency 1 :divisions 1}}
                 {0 [1] 1 [2]}))))}
    
  (let [total-frequency (reduce + 0 (map :frequency (vals day-types)))
        frequency-weight (/ 365.0 total-frequency)]
    (reduce
     (fn [year-total [day-type values]]
       (let [{:keys [frequency divisions]} (get day-types day-type)
             day-total  (reduce + 0.0 values)
             day-weight (* frequency 24.0 frequency-weight (/ divisions) )]
         (+ year-total (* day-total day-weight))))
     0.0
     quantity)))

(defn interpret-solution
  "Given `mip-sol`, which is the solved MIP associated with `problem`,
  interpret its variables to give information about the solution.

  the result will have :plant, :storage, and :curtailment in it, and should be a valid
  `thermos.opt.supply.specs/supply-solution`
  "
  [mip-sol problem]
  (binding [*discount-rate*     (:discount-rate problem 0)
            *accounting-period* (:accounting-period problem 1)]
    (let [vars (:vars mip-sol)

          profiled-value
          (fn [get-value]
            (into
             {}
             (for [[day-type {divisions :divisions}] (:profile problem)]
               [day-type (mapv get-value (repeat day-type) (range divisions))])))

          profiled-value-*
          (fn [profiled-value factor]
            (s/transform
             [(s/putval factor) s/MAP-VALS s/ALL] *
             profiled-value))
          
          ]
      
      {:plant
       (into
        {}
        (for [[plant-type {:keys [heat-efficiency chp power-efficiency fuel]}]
              (:plant-options problem)
              :when (-> vars :BUILD-PLANT :value (get plant-type false))]
          [plant-type
           (let [build          (-> vars :BUILD-PLANT :value (get plant-type false))
                 capacity-kw    (-> vars :PLANT-SIZE-KWP :value (get plant-type 0.0))
                 heat-output    (-> vars :HEAT-OUTPUT-KW :value)
                 power-per-heat (if power-efficiency
                                  (/ power-efficiency heat-efficiency)
                                  0)
                 
                 output (profiled-value
                         (fn [day-type division]
                           (get heat-output [plant-type [day-type division]] 0.0)))
                 
                 input (profiled-value
                        (fn [day-type division]
                          (/ (get heat-output [plant-type [day-type division]] 0.0)
                             heat-efficiency)))

                 generation
                 (when chp
                   (profiled-value-* output power-per-heat))

                 fuel-product ;; this function produces rates not
                              ;; absolute values so cannot be summed
                              ;; up directly. Sum-over-profile will do
                              ;; this right.
                 (fn [factor]
                   (profiled-value
                    (fn [day-type division]
                      (* (get heat-output [plant-type [day-type division]] 0.0)
                         (/ heat-efficiency)
                         (-> (:profile problem)
                             (get day-type)
                             (:fuel)
                             (get fuel) ;; from the (for {...} above)
                             (factor)
                             (get division))))))

                 span (-> problem :accounting-period)
                 
                 fuel-cost (fuel-product :price)
                 nox       (fuel-product :nox)
                 co2       (fuel-product :co2)
                 pm25      (fuel-product :pm25)

                 summarise-emission
                 (fn [type price]
                   (let [profile     (fuel-product type)
                         annual      (sum-over-profile (:profile problem) profile)
                         annual-cost (* annual price)
                         total       (* annual span)
                         total-cost  (* annual-cost span)
                         ]
                     {:annual-emission annual
                      :total-emission  total
                      :annual-cost     annual-cost
                      :total-cost      total-cost
                      :present-cost    (pv-recurring annual-cost)}))
                 
                 kwh-output-per-yr (sum-over-profile (:profile problem) output)
                 fuel-cost-per-yr  (sum-over-profile (:profile problem) fuel-cost)
                 
                 capital-cost (plant-capital-cost
                               problem plant-type
                               capacity-kw
                               kwh-output-per-yr)

                 operating-cost (plant-operating-cost
                                 problem plant-type
                                 capacity-kw
                                 kwh-output-per-yr)

                 grid-sales
                 (when chp
                   (profiled-value
                    (fn [day-type division]
                      (* power-per-heat
                         (get heat-output [plant-type [day-type division]] 0.0)
                         (-> (:profile problem)
                             (get day-type)
                             (:grid-offer)
                             (get division))
                         ))))

                 grid-revenue-per-yr (sum-over-profile (:profile problem) grid-sales)
                 ]
             (cond->
                 {:build build}
               
               build
               (assoc
                :build          build
                :capacity-kw    capacity-kw
                :output         output
                :input          input
                :capital-cost   capital-cost
                :operating-cost operating-cost
                :fuel-cost      {:annual-cost  fuel-cost-per-yr
                                 :present-cost (pv-recurring fuel-cost-per-yr)
                                 :total-cost   (* fuel-cost-per-yr span)}
                :output-kwh     kwh-output-per-yr
                :emissions
                {:nox  (summarise-emission :nox  (:nox-price problem))
                 :co2  (summarise-emission :co2  (:co2-price problem))
                 :pm25 (summarise-emission :pm25 (:pm25-price problem))})
               
               chp
               (assoc :generation   generation
                      :grid-revenue
                      {:annual-cost  (- grid-revenue-per-yr)
                       :total-cost   (* span (- grid-revenue-per-yr))
                       :present-cost (- (pv-recurring grid-revenue-per-yr))
                       }
                      )))
           ]))
       
       :storage
       (into
        {}
        (for [storage-type (keys (:storage-options problem))
              :when        (-> vars :BUILD-STORE :value (get storage-type false))
              ]
          [storage-type
           {:capacity-kwh
            (-> vars :STORE-SIZE-KWH :value (get storage-type 0.0))
            :capacity-kw
            (-> vars :STORE-SIZE-KWP :value (get storage-type 0.0))
            :output
            (let [charge (-> vars :FLOW-OUT-KW :value)]
              (profiled-value
               (fn [day-type division]
                 (get charge [storage-type [day-type division]] 0.0))))
            :input
            (let [charge (-> vars :FLOW-IN-KW :value)]
              (profiled-value
               (fn [day-type division]
                 (get charge [storage-type [day-type division]] 0.0))))
            :capital-cost
            (store-capital-cost
             problem storage-type
             (-> vars :STORE-SIZE-KWP :value (get storage-type 0.0))
             (-> vars :STORE-SIZE-KWH :value (get storage-type 0.0)))
            }]))
       
       :curtailment
       (into
        {}
        (let [curtailment (-> vars :CURT-KW :value)]
          (profiled-value
           (fn [day-type division]
             (get curtailment [day-type division] 0.0)))))})))

