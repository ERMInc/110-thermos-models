(ns thermos.opt.net.core
  "Thermos network optimisation model. Translated from the python version."
  (:require [lp.scip :as scip]
            [com.rpl.specter :as s]))

(defn- as-edge
  ([i j] (if (< i j) [i j] [j i]))
  ([[i j]] (as-edge i j)))

(defn- rev-edge [e] [(second e) (first e)])

(defn construct-mip [problem]
  (let [;; indexing sets
        vtx        (set (map :id (:vertices problem)))
        edge       (set (for [e (:edges problem)] (as-edge (:i e) (:j e))))

        arc        (into edge (map rev-edge edge))
        
        svtx       (set (map :id (filter :supply (:vertices problem))))
        dvtx       (set (map :id (filter :demand (:vertices problem))))
        emission   (set (keys (:emissions problem)))
        period     #{:peak :mean}

        alt-types  (set (mapcat (comp keys :alternatives) (:vertices problem)))
        ins-types  (set (mapcat (comp keys :insulation)   (:vertices problem)))

        ;; constants
        flow-bound-slack (:flow-bound-slack problem 1.5)

        hours-per-year (* 24.0 365)
        years-per-hour (/ 1.0 hours-per-year)

        vertices   (assoc-by :id (:vertices problem))
        emissions  (:emissions problem)
        arc-map    (merge (assoc-by (comp vec (juxt :i :j)) (:edges problem))
                          (assoc-by (comp vec (juxt :j :i)) (:edges problem)))
        
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
        
        neighbours (into {} (for [[i ijs] (group-by first arc)]
                              [i (set (map second ijs))]))
        
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
                flow-in  [+ (for [j neighbours] [:ARC-FLOW-KW [j i] t])]
                flow-out [+ (for [j neighbours] [:ARC-FLOW-KW [i j] t])]

                losses   (if (= :mean period)
                           [+ (for [j neighbours] [* [:LOSS-KW (as-edge i j)] [:AIN [j i]]])]
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

        ;; Utilities for computing parameters & bounds
        edge-length (-> (for [[a e] arc-map] [a (:length e)]) (into {}))

        loss-w-per-kwp (interpolate
                        (-> problem :pipe-losses (:kwp     [0]))
                        (-> problem :pipe-losses (:w-per-m [0])))
        
        edge-loss-kw-for-kwp
        (fn [e kwp]
          (* (edge-length e) (/ (loss-w-per-kwp kwp) 1000.0)))

        max-loss-kw
        (reduce
         +
         (for [e edges]
           (let [[[_ max-fwd] [_ max-back]]
                 (-> (get arc-map e) :bounds :peak)
                 max-fwd (or max-fwd 0)
                 max-back (or max-back 0)]
             (edge-loss-kw-for-kwp e (max max-fwd max-back)))))
        
        arc-max-mean-flow
        (into
         {}
         (for [a arc]
           (let [edge (get arc-map a)
                 [[_ max-fwd] [_ max-back]] (-> edge :bounds :mean)]
             ;; question here is whether a is same direction or reverse direction
             [a (if (= (:i edge) (first a)) max-fwd max-back)])))
        
        arc-max-peak-flow
        (into
         {}
         (for [a arc]
           (let [edge (get arc-map a)
                 [[_ max-fwd] [_ max-back]] (-> edge :bounds :peak)]
             ;; question here is whether a is same direction or reverse direction
             [a (if (= (:i edge) (first a)) max-fwd max-back)])))

        flow-upper-bound (fn [a p]
                           (case p
                             :mean (+ max-loss-kw (arc-max-mean-flow a))
                             :peak (arc-max-peak-flow a)))

        diversity (diversity-factor problem)

        total-count (reduce + (map #(:count % 1) (:vertices problem)))
        
        initial-supply-diversity (diversity total-count)
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
        [<= [:ARC-FLOW-KW a t] [* [:AIN a] flow-bound-slack [:lp.core/upper [:ARC-FLOW-KW a t]]]])

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
         [>= [:ARC-FLOW-KW a :mean] [* [:AIN a] [:LOSS-KW (as-edge a)]]]
         ;; Edges have capacity for peak flow
         [>= [:EDGE-CAP-KW (as-edge a)] [* [:ARC-FLOW-KW a :peak] [:EDGE-DIVERSITY e]]]
         ;; Edges have capacity for mean flow
         [>= [:EDGE-CAP-KW (as-edge a)] [:ARC-FLOW-KW a :mean]]])
      
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
     (cond->
         {;; PARAMETERS (fixed = true)
          
          :EDGE-DIVERSITY
          {:indexed-by [edge] :fixed true
           :value ;; intial diversity is super-optimistic
           (fn [e]
             (let [ [ [_ fwd] [_ bwd] ] (-> arc-map (get e) :bounds :count)]
               [e (diversity (max fwd bwd))]))}
          
          :SUPPLY-DIVERSITY
          {:indexed-by [svtx] :fixed true :value initial-supply-diversity}
          
          :LOSS-KW
          {:indexed-by [edge] :fixed true
           :value
           (fn [e]
             (let [[[min-fwd _] [min-back _]] (-> (get arc-map e) :bounds :mean)
                   min-fwd  (or min-fwd 0)
                   min-back (or min-back 0)
                   min-flow (min min-fwd min-back)
                   kwp (if (zero? min-flow) (max min-fwd min-back) min-flow)]
               [e (edge-loss-kw-for-kwp e kwp)]))}

          ;; VARIABLES
          
          :DVIN {:type :binary :indexed-by [dvtx]
                 :value demand-is-required
                 :fixed demand-is-required}
          
          :AIN  {:type :binary :indexed-by [arc]}
          :SVIN {:type :binary :indexed-by [svtx]}

          :ARC-FLOW-KW {:type :non-negative :indexed-by [arc period]
                        :upper flow-upper-bound}
          
          :EDGE-CAP-KW {:type :non-negative :indexed-by [edge]
                        :upper edge-max-flow}
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
        ;; TODO restrict indices to valid combinations, since I can now
        {:INSULATION-KWH {:type :non-negative :indexed-by [dvtx ins-types]
                          :lower insulation-min-kwh
                          :upper insulation-max-kwh}
         :INSULATION-IN {:type :binary :indexed-by [dvtx ins-types]
                         :value (fn [i it] (when-not (insulation-allowed i it) false))
                         :fixed (fn [i it] (when-not (insulation-allowed i it) true))}
         }
        ))

     ;; OTHER JUNK, for use elsewhere.
     ::edge    edge
     ::edge-loss edge-loss-kw-for-kwp
     ::diversity diversity
     ::arc     arc
     ::svtx    svtx
     ::dvtx    dvtx
     ::arc-map arc-map
     ::vtx-map vertices
     }))

(defn- postorder [children root]
  (let [walk (fn walk [visited from node]
               (if (visited node)
                 [[::loop node]]
                 (concat
                  (mapcat (partial walk (conj visited node) node)
                          (children node))
                  [[::node node]]
                  (when from [[::edge from node]])
                  )))]
    (walk #{} nil root)))

(defn- count-up [root children f r]
  (reduce
   (fn [acc item]
     (case (first item)
       ::edge (assoc acc (vec (rest item)) (get acc (nth item 2)))
       ::loop acc
       ::node (let [x (second item)]
                (assoc acc x
                       (reduce r (cons (f x) (for [c (children x)] (get acc c 0))))))))
   {} (postorder children root)))

(defn- truthy [v] (or (= true v) (and (number? v) (> v 0.5))))

(defn- diversity-factor
  ([rate limit n]
   (/ (Math/round
       (* 100.0 (+ limit (/ (- 1.0 limit) (max 1.0 (* n rate))))))
      100.0))
  ([problem]
   (let [rate  (:diversity-rate problem 1.0)
         limit (:diversity-limit problem 0.62)]
     (fn [n] (diversity-factor rate limit n)))))

(defn- compute-parameters
  "The mip has been solved, so we can figure out the diversity & heat
  loss parameters for the solution."
  [mip]
  (let [diversity-factor (::diversity mip) ; Get hold of diversity function

        ;; Find which arcs went into the solution
        arcs-in (-> mip :vars :AIN :value
                    (->> (keep (fn [[a v]] (when (truthy v) a)))))

        ;; Transform to adjacency matrix
        adj     (reduce
                 (fn [acc [i j]] (update acc i conj j))
                 {}
                 arcs-in)

        ;; Find which supplies we used
        roots   (-> mip :vars :SVIN :value
                    (->> (keep (fn [[i v]] (when (truthy v) i)))))

        max-0 #(max (or %1 0) (or %2 0))

        vtx-map (::vtx-map mip)
        
        edge-counts                   ; the number of places
                                        ; reachable through each edge
        (apply merge-with max-0
               (for [s roots] (count-up s adj #(-> vtx-map (get %) (:count 1)) +)))
        
        edge-max-peak-flow            ; the largest individual peak
                                        ; demand through each edge
        (apply merge-with max-0
               (for [s roots] (count-up s adj #(-> vtx-map (get %) :demand (:kwp 0)) max)))

        flow-kw (-> lp :vars :ARC-FLOW-KW :value)
        edge-loss-kw (::edge-loss mip) ;; a function, worked out before
        
        edge-parameters                 ; For edges, losses &
                                        ; diversity worked out
                                        ; together
        (->>
         (for [e (::edge lp)]
           (let [count-a (edge-counts e 0)
                 count-b (edge-counts (rev-edge e) 0)

                 max-peak-a (edge-max-peak-flow e 0)
                 max-peak-b (edge-max-peak-flow (rev-edge e) 0)

                 count    (or count-a count-b)
                 max-peak (or max-peak-a max-peak-b)

                 diversity (diversity-factor count)
                 undiversified-flow (max
                                     (get flow-kw [e :peak])
                                     (get flow-kw [(rev-edge e) :peak]))

                 diversified-flow (* diversity undiversified-flow)
                 
                 diversity              ; This fixes a mistake where
                                        ; we diversify a pipe to carry
                                        ; less than the biggest load
                                        ; below it, which makes no
                                        ; sense
                 (if (and (> max-peak diversified-flow)
                          (pos? undiversified-flow))
                   (/ max-peak undiversified-flow)
                   diversity)

                 diversified-flow (* diversity undiversified-flow)

                 heat-loss (edge-loss-kw e diversified-flow)
                 ]
             
             [e [diversity heat-loss]]))
         (into {}))
        ]
    {:edge-diversity    (into {} (for [[e [d]] edge-parameters] [e d]))
     :edge-losses       (into {} (for [[e [_ l]] edge-parameters] [e l]))
     :supply-diversity  (into {} (for [s (::svtx lp)] [s (diversity-factor (edge-counts s 0.0))]))
     }))

(defn- parameterise
  "Given a MIP from construct-mip above (which may have a solution on it)
  Compute and install the values for :EDGE-DIVERSITY :SUPPLY-DIVERSITY and :LOSS-KW
  which are not determined within the program. "

  [mip]
  (if (:solution mip)
    (let [{& :keys [edge-losses edge-diversity supply-diversity]} (compute-parameters mip)]
      (s/multi-transform
       [:vars
        (s/multi-path
         [:EDGE-DIVERSITY   :value (s/terminal-val edge-diversity)]
         [:SUPPLY-DIVERSITY :value (s/terminal-val supply-diversity)]
         [:LOSS-KW          :value (s/terminal-val edge-losses)])]))

    mip ;; if no solution, stick with initial parameters
    ))

(let [fix-decision (fn [var]
                     (assoc var
                            :fixed true
                            ::was-fixed (:fixed var)))
      unfix-decision (fn [var]
                       (-> var
                           (assoc :fixed (::was-fixed var))
                           (dissoc ::was-fixed)))
      ]

  (defn- fix-decisions [mip]
    (s/multi-transform
     [:vars
      (s/multi-path
       [:AIN (s/terminal fix-decision)]
       [:DVIN (s/terminal fix-decision)]
       [:SVIN (s/terminal fix-decision)]
       [:INSULATION-IN (s/terminal fix-decision)]
       [:INSULATION-KWH (s/terminal fix-decision)]
       [:ALT-IN (s/terminal fix-decision)])]
     mip))
  
  (defn- fix-decisions [mip]
    (s/multi-transform
     [:vars
      (s/multi-path
       [:AIN (s/terminal unfix-decision)]
       [:DVIN (s/terminal unfix-decision)]
       [:SVIN (s/terminal unfix-decision)]
       [:INSULATION-IN (s/terminal unfix-decision)]
       [:INSULATION-KWH (s/terminal unfix-decision)]
       [:ALT-IN (s/terminal unfix-decision)])]
     mip)))

(defn- summary-decisions [mip]
  (let [vars (:vars mip)]
    (vec (for [k [:AIN :DVIN :SVIN :INSULATION-IN :INSULATION-KWH :ALT-IN]]
           (-> vars (get k) :value)))))

(defn- summary-parameters [mip]
  ;; edge diversity, supply diversity to 2dp, edge loss to nearest kw
  (let [ed (-> mip :vars :EDGE-DIVERSITY :value)
        el (-> mip :vars :LOSS-KW :value)
        sd (-> mip :vars :SUPPLY-DIVERSITY :value)
        r #(/ (Math/round (* 100.0 (or % 0))) 100.0)
        ]
    [(for [e (::edge mip)]
       [e [(r (ed e)) (Math/round (el e))]])
     (for [s (::svtx mip)]
       [s (r (sd s))])]))

(defn- solve [mip & {:keys [mip-gap time-limit]}]
  (let [sol-free (scip/solve mip :time-limit time-limit :mip-gap mip-gap)
        sol-fix  (-> sol-free (parameterise) ;; reparameterise
                     (fix-decisions) (scip/solve) (unfix-decisions))

        stable
        (= (summary-parameters sol-free)
           (summary-parameters sol-fix))
        ]
    
    ;; Copy the gap & bounds from the free solution, as there's no gap
    ;; or bounds for the fixed one as we have fixed it.
    (s/multi-transform
     [:solution
      (s/multi-path
       [:stable (s/terminal-val stable)]
       [:gap    (s/terminal-val (-> sol-free :solution :gap))]
       [:bounds (s/terminal-val (-> sol-free :solution :bounds))]
       )]
     sol-fix)))

(defn run-model [problem]
  (let [mip (construct-mip problem)
        iteration-limit (:iteration-limit problem 100000)
        time-limit (:time-limit problem 1.0)
        mip-gap    (:mip-gap problem 0.05)

        end-time (+ (* time-limit 1000 3600)
                    (System/currentTimeMillis))

        most-negative (- Double/MAX_VALUE)
        ]
    
    (loop [mip   mip ;; comes parameterised out of the gate
           seen  #{} ;; decision sets we have already seen
           iters 0   ;; number of tries
           best  nil ;; best so far
           ]
      (let [solved-mip (solve mip
                              :mip-gap mip-gap
                              :time-limit
                              (max 60 (/ (- end-time (System/currentTimeMillis)) 1000.0)))
            
            decisions  (summary-decisions solved-mip)
            
            best       (if (> (-> solved-mip :solution (:objective most-negative))
                              (-> best       :solution (:objective most-negative)))
                         solved-mip (or best solved-mip))

            is-stable  (:stable (:solution solved-mip))
            has-looped (contains? seen decisions)
            out-of-iters (> iteration-limit iters)
            out-of-time (> (System/currentTimeMillis) end-time)
            ]

        (when is-stable    (println "Solution is stable"))
        (when has-looped   (println "Solution is looping"))
        (when out-of-iters (println "Iteration limit reached"))
        (when out-of-time  (println "Time limit reached"))
        
        (if (or has-looped out-of-iters out-of-time is-stable)
          best
          (recur solved-mip (conj seen decisions) (inc iters) best)
          )))))
