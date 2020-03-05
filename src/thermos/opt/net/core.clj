(ns thermos.opt.net.core
  "Thermos network optimisation model. Translated from the python version.")

(defn construct-mip [problem]
  (let [;; indexing sets here
        edge (fn edge
               ([i j] (if (< i j) [i j] [j i]))
               ([[i j]] (edge i j)))
        
        vtx        (set (map :id (:vertices problem)))
        edges      (set (for [e (:edges problem)]
                          (edge (:i e) (:j e))))
        
        arcs       (set (concat edges (map (comp vec reverse) edges)))
        
        svtx       (set (map :id (filter :supply (:vertices problem))))
        dvtx       (set (map :id (filter :demand (:vertices problem))))
        emission   (set (keys (:emissions problem)))
        period     #{:peak :mean}

        alt-types  (set (mapcat (comp keys :alternatives) (:vertices problem)))
        ins-types  (set (mapcat (comp keys :insulation)   (:vertices problem)))

        ;; constants
        flow-bound-slack (:flow-bound-slack problem 1.5)

        flow-upper-bound (fn [a t])

        hours-per-year (* 24.0 365)
        years-per-hour (/ 1.0 hours-per-year)

        vertices   (assoc-by :id (:vertices problem))
        emissions  (:emissions problem)
        arc-map    (merge (assoc-by (juxt :i :j) (:edges problem))
                          (assoc-by (juxt :j :i) (:edges problem)))
        
        ;; accessor functions
        demand-kwp (fn [i] (or (-> vertices (get i) :demand :kwp) 0))
        demand-kwh (fn [i] (or (-> vertices (get i) :demand :kwh) 0))

        demand-is-required
        (fn [i] (boolean (-> vertices (get i) :demand :required)))

        demand-kw  (fn [i t]
                     (if (= :mean t)
                       (/ (demand-kwh i) hours-per-year)
                       (demand-kwp i)))

        edge-fixed-cost
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (:cost-per-m e 0)) 0)))

        edge-cost-per-kwp
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (:cost-per-kwm e 0)) 0)))

        supply-max-capacity (fn [i] (or (-> (vertices i) :supply :capacity-kw) 0))
        supply-fixed-cost   (fn [i] (or (-> (vertices i) :supply :cost) 0))
        supply-cost-per-kwh (fn [i] (or (-> (vertices i) :supply :cost-per-kwh) 0))
        supply-cost-per-kwp (fn [i] (or (-> (vertices i) :supply :cost-per-kwp) 0))
        supply-emissions-per-kw (fn [i e]
                                  (* (or (-> (vertices i) :suppy :emissions (get e)) 0)
                                     hours-per-year))
        

        vertex-fixed-value   (fn [i] (or (-> (vertices i) :demand :value) 0))
        vertex-value-per-kwp (fn [i] (or (-> (vertices i) :demand :value-per-kwp) 0))
        vertex-value-per-kwh (fn [i] (or (-> (vertices i) :demand :value-per-kwh) 0))
        
        neighbours (for [[i ijs] (group-by first arcs)]
                     [i (set (map second ijs))])
        
        supply-count-max         (:supply-limit problem)

        emissions-cost-per-kg    (fn [e] (or (-> (emissions e) :cost) 0))
        emissions-limit          (fn [e] (-> (emissions e) :limit))

        insulation-attr          (fn [i it a]
                                   (-> (vertices i) :demand :insulation
                                       (get it) (get a)))

        insulation-allowed       (fn [i it]
                                   (-> (vertices i) :demand :insulation (contains? it)))
        insulation-fixed-cost    (fn [i it] (or (insulation-attr i it :cost) 0))
        insulation-cost-per-kwh  (fn [i it] (or (insulation-attr i it :cost-per-kwh) 0))
        insulation-max-kwh       (fn [i it] (or (insulation-attr i it :maximum) 0))
        insulation-min-kwh       (fn [i it] (or (insulation-attr i it :minimum) 0))

        alternative-attr         (fn [i at a]
                                   (-> (vertices i) :demand :alternatives (get at) (get a)))

        alternative-allowed      (fn [i a]
                                   (-> (vertices i) :demand :alternatives (contains? a)))
        
        alternative-fixed-cost   (fn [i a] (or (alternative-attr i a :cost) 0))
        alternative-cost-per-kwp (fn [i a] (or (alternative-attr i a :cost-per-kwp) 0))
        alternative-cost-per-kwh (fn [i a] (or (alternative-attr i a :cost-per-kwh) 0))
        alternative-emissions-per-kwh (fn [i a e]
                                        (or (-> (vertices i) :demand :alternatives (get a)
                                                :emissions (get e))
                                            0))

        total-max-insulation     (into {} (for [[i v] vertices]
                                            [i (-> v :demand :insulation vals
                                                   (->> (keep :maximum)
                                                        (reduce + 0)))]))
        
        ;; Some common subexpressions:
        
        total-emissions
        (fn [e]
          [+
           (for [i svtx] [* [:SUPPLY-KW i :mean] (supply-emissions-per-kw i e)])
           (for [i dvtx a alt-types :let [f (alternative-emissions-per-kwh i a e)]]
             [-
              [* [:ALT-IN i a] (demand-kwh i) f]
              [* [:ALT-AVOIDED-KWH i a] f]])])
        
        unmet-demand
        (fn [i t]
          (let [neighbours (neighbours i)
                flow-in  [+ (for [j neighbours] [:FLOW-KW [j i] t])]
                flow-out [+ (for [j neighbours] [:FLOW-KW [i j] t])]

                losses   (if (= :mean period)
                           [+ (for [j neighbours] [* [:LOSS-KW (edge i j)] [:AIN [j i]]])]
                           0)

                demand (demand-kw i t)

                supply (if (contains? svtx i) [:SUPPLY-KW i t] 0)
                ]

            [- [+ demand flow-out losses] [+ supply flow-in]]))

        total-connection-value
        [+ (for [i dvtx]
             [-
              [*
               [:DVIN i]
               (+ (vertex-fixed-value i)
                  (* (demand-kwh i) (vertex-value-per-kwh i))
                  (* (demand-kwp i) (vertex-value-per-kwp i)))]

              ;; we don't get paid for unmet demand
              ;; we don't want to multiply it by DVIN though, since that's quadratic
              (* (unmet-demand i :mean)
                 hours-per-year
                 (vertex-value-per-kwh i))])]

        total-supply-cost
        [+ (for [i svtx]
             [+
              [* [:SVIN i] (supply-fixed-cost i)]
              [* [:SUPPLY-CAP-KW i] (max 0.01 (supply-cost-per-kwp i))]
              [* [:SUPPLY-KW i :mean] (max 0.01 (supply-cost-per-kwh i)) hours-per-year]])]

        total-pipe-cost
        [+ (for [e edge :let [[i j] e]]
             [+
              [* [+ [:AIN [i j]] [:AIN [j i]]] (edge-fixed-cost e)]
              [* [:EDGE-CAP-KW e] (max 0.01 (edge-cost-per-kwp e))]])]
        
        emissions-cost
        [+ (for [e emission]
             [*
              (emissions-cost-per-kg e)
              (total-emissions e)])]

        total-insulation-cost
        [+ (for [i dvtx it ins-types]
             [+
              [* [:INSULATION-IN i it] (insulation-fixed-cost i it)]
              [* [:INSULATION-KWH i it] (insulation-cost-per-kwh i it)]])]

        total-alt-cost
        [+ (for [i dvtx a alt-types]
             [-
              [* [:ALT-IN i a]
               (+ (alternative-fixed-cost i a)
                  (* (alternative-cost-per-kwp i a) (demand-kwp i))
                  (* (alternative-cost-per-kwh i a) (demand-kwh i)))]

              ;; don't pay for what we didn't use due to insulation
              [* [:ALT-AVOIDED-KWH i a] (alternative-cost-per-kwh i a)]])]

        avoided-demand-kwh
        (->> (for [i dvtx]
               [i [+ (for [it ins-types] [:INSULATION-KWH i it])]])
             (into {}))

        ;; wrap in a function so we can return 0 in case nothing there.
        avoided-demand-kwh #(get avoided-demand-kwh % 0.0)
        ]
    {:maximize
     [- total-connection-value
      [+
       total-supply-cost
       total-pipe-cost
       emissions-cost
       total-insulation-cost
       total-alt-cost]]
     
     :subject-to
     (list
      ;; Flow only goes one way
      (for [e edge]
        [<= [+ [:AIN (vec e)] [:AIN (reverse (vec e))]] 1])

      ;; force AIN if we use flow
      (for [a arc t period]
        [<= [:FLOW-KW a t] [* [:AIN a] flow-bound-slack (flow-upper-bound a t)]])

      ;; Flow balance at each vertex
      (for [i vtx t period]
        [<=
         0
         (unmet-demand i t)
         (if (and [= :mean t] (contains? dvtx i))
           [* (avoided-demand-kwh i) years-per-hour]
           0)])

      ;; Constraints for arcs
      (for [a arc]
        [:and
         ;; Arcs carry their losses
         [>= [:FLOW-KW a :mean] [* [:AIN a] [:LOSS-KW (edge a)]]]
         ;; Edges have capacity for peak flow
         [>= [:EDGE-CAP-KW (edge a)] [* [:FLOW-KW a :peak] [:EDGE-DIVERSITY e]]]
         ;; Edges have capacity for mean flow
         [>= [:EDGE-CAP-KW (edge a)] [:FLOW-KW a :mean]]])
      
      ;; supply capacity sufficient
      (for [i svtx]
        [:and
         [>= [:SUPPLY-CAP-KW i] [* [:SUPPLY-KW i :peak] [:SUPPLY-DIVERSITY i]]]
         [>= [:SUPPLY-CAP-KW i] [:SUPPLY-KW i :mean]]
         [<= [:SUPPLY-CAP-KW i] [* [:SVIN i] (supply-max-capacity i)]]
         [<= [:SUPPLY-CAP-KW i] (supply-max-capacity i)] ;; redundant really
         ])

      ;; not too many supplies
      (when supply-count-max
        [<= [+ (for [i svtx] [:SVIN i])] supply-count-max])
      
      ;; emissions limits
      (for [e emission
            :let [lim (emissions-limit e)] :when lim]
        [<= (total-emissions e) lim])

      ;; rules for alternatives
      ;; 1. we must pick a heating system
      (for [i dvtx
            :when (not-empty (vertex-alternatives i))]
        [= 1 [+
              [:DVIN i]
              (for [a alt-types] [:ALT-IN i a])]])

      (for [i dvtx a alt-types]
        [:and
         ;; 2. We can avoid as much as insulation allows us to
         [<= [:ALT-AVOIDED-KWH i a] (avoided-demand-kwh i)]
         ;; 3. But only if we are actually using this alt.
         [<= [:ALT-AVOIDED-KWH i a] [* [:ALT-IN i a] (demand-kwh i)]]])

      ;; rules for insulation:
      ;; 1. Big-M constraint to toggle payment of fixed cost.
      (for [i dvtx it ins-types]
        [<= [:INSULATION-KWH i it] [* (insulation-max-kwh i it) [:INSULATION-IN i it]]])
      
      )
     
     :vars
     ;; TODO fixed values
     (cond->
         { ;; really these are params not vars
          :EDGE-DIVERSITY   {:indexed-by [edge] :fixed true}
          :SUPPLY-DIVERSITY {:indexed-by [svtx] :fixed true}
          :LOSS-KW          {:indexed-by [edge] :fixed true}

          :DVIN {:type :binary :indexed-by [dvtx]
                 :value demand-is-required
                 :fixed demand-is-required}
          
          :AIN  {:type :binary :indexed-by [arcs]}
          :SVIN {:type :binary :indexed-by [svtx]}

          :FLOW-KW {:type :non-negative :indexed-by [arc period]}
          :EDGE-CAP-KW {:type :non-negative :indexed-by [edge]}
          :SUPPLY-CAP-KW {:type :non-negative :indexed-by [svtx]}
          :SUPPLY-KW {:type :non-negative :indexed-by [svtx]}}

       (not-empty alt-types)
       (merge
        ;; TODO restrict indices to valid combinations
        {:ALT-IN          {:type :binary :indexed-by [dvtx alt-types]
                           :value (fn [i a] (when-not (alternative-allowed i a) false))
                           :fixed (fn [i a] (when-not (alternative-allowed i a) true))}
         
         :ALT-AVOIDED-KWH {:type :non-negative :indexed-by [dvtx alt-types]
                           :upper (fn [i a]
                                    (if (alternative-allowed i a)
                                      (total-max-insulation i) ;; tighten to insulation max
                                      0))
                           :lower 0}})

       (not-empty ins-types)
       (merge
        ;; TODO restrict indices to valid combinations
        {:INSULATION-KWH {:type :non-negative :indexed-by [dvtx ins-types]
                          :lower insulation-min-kwh
                          :upper insulation-max-kwh}
         :INSULATION-IN {:type :binary :indexed-by [dvtx ins-types]
                         :value (fn [i it] (when-not (insulation-allowed i it) false))
                         :fixed (fn [i it] (when-not (insulation-allowed i it) true))}
         }
        ))
     }))

(defn- compute-edge-diversity
  "Return a map {[i j] => diversity}"
  [mip])

(defn- compute-supply-diversity
  "Return a map {i => diversity}."

  [mip])

(defn- compute-edge-losses
  "Return a map {[i j] => losses-kw}"
  [mip])

(defn parameterise
  "Given a MIP from construct-mip above (which may have a solution on it)
  Compute and install the values for :EDGE-DIVERSITY :SUPPLY-DIVERSITY and :LOSS-KW
  which are not determined within the program. "

  [mip]
  (update mip :vars
          #(-> %
               (assoc-in [:EDGE-DIVERSITY :value]   (compute-edge-diversity mip))
               (assoc-in [:SUPPLY-DIVERSITY :value] (compute-supply-diversity mip))
               (assoc-in [:LOSS-KW :value]          (compute-edge-losses mip)))))
