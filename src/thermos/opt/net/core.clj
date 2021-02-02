(ns thermos.opt.net.core
  "Thermos network optimisation model. Translated from the python version."
  (:require [lp.scip :as scip]
            [com.rpl.specter :as s]
            [lp.core :as lp]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [thermos.opt.net.specs :refer [network-problem]]
            [clojure.spec.alpha :as spec]
            [thermos.opt.net.diversity :refer [diversity-factor]]
            [thermos.opt.net.bounds :as bounds]))

(def ^:const hours-per-year (* 24.0 365))

(def ^:const years-per-hour (/ 1.0 hours-per-year))

(defn- as-edge
  ([i j] (if (neg? (compare i j)) [i j] [j i]))
  ([[i j]] (as-edge i j)))

(defn- rev-edge [e] [(second e) (first e)])

(defn assoc-by [f s]
  (reduce #(assoc %1 (f %2) %2)  {} s))

(defn- interpolate [xs ys]
  (let [curve (mapv vector xs ys)]
    (fn [x]
      (let [position (java.util.Collections/binarySearch curve [x]
                                                         #(<= (first %1)
                                                              (first %2)))]
        (let [position (if (neg? position)
                         (- (- position) 1)
                         position)]
          (if (= position (count curve))
            (second (last curve))

            (let [[px py] (nth curve position)]
              (if (or (== x px) (zero? position))
                py
                (let [[px2 py2] (nth curve (dec position))
                      m (/ (- py py2) (- px px2))
                      fr (- x px2)]
                  (+ py2 (* fr m)))))))))))

(defn- valid? [x y]
  (let [is-valid (spec/valid? x y)]
    (when-not is-valid
      (log/error (spec/explain-str x y)))
    is-valid))

(defn construct-mip [problem]
  {:pre [(valid? network-problem problem)]}
  (let [flow-bounds (or (:bounds problem)
                        (bounds/compute-bounds problem))

        ;; regroup insulation and alternatives
        problem    (s/multi-transform
                    [(s/putval :id)

                     :vertices
                     s/ALL
                     
                     (s/multi-path
                      [:demand :alternatives (s/terminal assoc-by)]
                      [:demand :insulation   (s/terminal assoc-by)]
                      )]
                    problem)

        ;; indexing sets
        edge       (set (for [e (:edges problem)] (as-edge (:i e) (:j e))))
        vtx        (into (set (map :id (:vertices problem)))
                         (mapcat identity edge))
        
        arc        (into edge (map rev-edge edge))
        
        svtx       (set (map :id (filter :supply (:vertices problem))))
        dvtx       (set (map :id (filter :demand (:vertices problem))))
        emission   (set (keys (:emissions problem)))
        period     #{:peak :mean}

        ;; this should give us a list of sets - each set contains the IDs
        ;; of some dvtx which have to go on or off together
        grouped-demands (->> (:vertices problem)
                             (filter (comp :group :demand))
                             (group-by (comp :group :demand))
                             (s/transform [s/MAP-VALS s/ALL] :id)
                             (map second)
                             (map set))
        
        alt-types  (set (mapcat (comp keys :alternatives :demand) (:vertices problem)))
        ins-types  (set (mapcat (comp keys :insulation :demand)   (:vertices problem)))

        ;; constants
        flow-bound-slack (:flow-bound-slack problem 1.5)

        vertices   (assoc-by :id (:vertices problem))
        emissions  (:emissions problem)
        arc-map    (merge (assoc-by (comp vec (juxt :i :j)) (:edges problem))
                          (assoc-by (comp vec (juxt :j :i)) (:edges problem)))
        
        ;; accessor functions
        demand-kwp (fn [i] (or (-> vertices (get i) :demand :kwp) 0))
        demand-kwh (fn [i] (or (-> vertices (get i) :demand :kwh) 0))

        demand-is-required
        (fn [i] (boolean (-> vertices (get i) :demand :required)))

        edge-fixed-cost
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (get e :cost%m 0)) 0)))

        edge-cost-per-kwp
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (get e :cost%kwm 0)) 0)))

        supply-max-capacity (fn [i] (or (-> (vertices i) :supply :capacity-kw) 0))
        supply-fixed-cost   (fn [i] (or (-> (vertices i) :supply :cost) 0))
        supply-cost-per-kwh (fn [i] (or (-> (vertices i) :supply (get :cost%kwh)) 0))
        supply-cost-per-kwp (fn [i] (or (-> (vertices i) :supply (get :cost%kwp)) 0))
        supply-emissions-per-kw (fn [i e]
                                  (* (or (-> (vertices i) :suppy :emissions (get e)) 0)
                                     hours-per-year))
        

        vertex-fixed-value   (fn [i] (or (-> (vertices i) :demand :value) 0))
        vertex-value-per-kwp (fn [i] (or (-> (vertices i) :demand (get :value%kwp)) 0))
        vertex-value-per-kwh (fn [i] (or (-> (vertices i) :demand (get :value%kwh)) 0))
        vertex-alternatives  (fn [i]
                               (-> (vertices i) :demand :alternatives keys set))

        vertex-demand-count  (fn [i] (if (dvtx i)
                                       (-> (vertices i) :demand (:count 1))
                                       0))
        
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
        insulation-cost-per-kwh  (fn [i it] (or (insulation-attr i it :cost%kwh) 0))
        insulation-max-kwh       (fn [i it] (or (insulation-attr i it :maximum) 0))
        insulation-min-kwh       (fn [i it] (or (insulation-attr i it :minimum) 0))

        alternative-attr         (fn [i at a]
                                   (-> (vertices i) :demand :alternatives (get at) (get a)))

        alternative-allowed      (fn [i a]
                                   (-> (vertices i) :demand :alternatives (contains? a)))
        
        alternative-fixed-cost   (fn [i a] (or (alternative-attr i a :cost) 0))
        alternative-cost-per-kwp (fn [i a] (or (alternative-attr i a :cost%kwp) 0))
        alternative-cost-per-kwh (fn [i a] (or (alternative-attr i a :cost%kwh) 0))
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

        diversity (diversity-factor problem)
        
        unmet-demand
        (let [demand-kw  (fn [i t]
                           (if (= :mean t)
                             (/ (demand-kwh i) hours-per-year)

                             ;; The reason for the ratio below is that an individual building can contain many addresses
                             ;; but the demand recorded for it is the actual peak (connection size required).
                             ;; Since we will later apply a diversity factor to all pipes according to their counts
                             ;; we must first take the factor off here so when we put it back on later all is well.
                             ;; This is done here instead of as a preprocessing step because we only want to do it for
                             ;; networks - for individual systems we want the diversified value.

                             ;; Since we only want this happening in here, demand-kw is closed over by unmet-demand.
                             ;; An analogous change happens in flow-bounds calculation in bounds.clj.
                             ;; This is to make sure that, if we are sending un-diversified demand up a pipe
                             ;; we have an un-diversified upper bound for that pipe's capacity
                             (/ (demand-kwp i)
                                (diversity (vertex-demand-count i)))))
              ]
          (fn [i t]
            (let [neighbours (neighbours i)
                  flow-in  [:+ (for [j neighbours] [:ARC-FLOW-KW [j i] t])]
                  flow-out [:+ (for [j neighbours] [:ARC-FLOW-KW [i j] t])]

                  losses   (if (= :mean t)
                             [:+ (for [j neighbours]
                                   [:* [:LOSS-KW (as-edge i j)] [:AIN [j i]]])]
                             0)

                  demand (if (contains? dvtx i)
                           [:* [:DVIN i] (demand-kw i t)]
                           0.0)

                  supply (if (contains? svtx i) [:SUPPLY-KW i t] 0)
                  ]
              [:- [:+ demand flow-out losses] [:+ supply flow-in]])))

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
              [* (unmet-demand i :mean)
                 hours-per-year
                 (vertex-value-per-kwh i)]])]

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
               [i (if (seq ins-types)
                    [+ (for [it ins-types] [:INSULATION-KWH i it])]
                    0.0)])
             (into {}))

        ;; wrap in a function so we can return 0 in case nothing there.
        avoided-demand-kwh #(get avoided-demand-kwh % 0.0)

        ;; Utilities for computing parameters & bounds
        edge-length   (->> (for [[a e] arc-map] [a (:length e)]) (into {}))
        edge-required (->> (for [[a e] arc-map] [a (:required e)]) (into {}))

        loss-w-per-kwp (memoize
                        (interpolate
                         (-> problem :pipe-losses (:kwp     [0]))
                         (-> problem :pipe-losses (get :w%m [0]))))
        
        edge-loss-kw-for-kwp
        (fn [e kwp]
          (* (edge-length e)
             (/ (loss-w-per-kwp kwp) 1000.0)))

        max-loss-kw
        (reduce
         +
         (for [e edge]
           (let [max-fwd  (:peak-max (get flow-bounds e) 0)
                 max-back (:peak-max (get flow-bounds (rev-edge e)) 0)]
             (edge-loss-kw-for-kwp e (max max-fwd max-back)))))

        arc-max-mean-flow
        (into
         {} (for [a arc] [a (:mean-max (get flow-bounds a) 0)]))
        
        arc-max-peak-flow
        (into
         {} (for [a arc] [a (:peak-max (get flow-bounds a) 0)]))

        edge-max-flow ;; this is the max capacity when diversified.
        (fn [e] (-> (arc-map e) (:max-capacity%kwp 100000.0)))
        
        flow-upper-bound ;; this is our best guess on the max
                         ;; un-diverse flow on this arc
        (fn [a p]
          (let [arc-bound
                (case p
                  :mean (+ (arc-max-mean-flow a) max-loss-kw)
                  :peak (arc-max-peak-flow a))]
            (if (zero? arc-bound) 0
                (* flow-bound-slack arc-bound))))
        
        total-count (reduce + (map #(:count (:demand %) 1) (filter :demand (:vertices problem))))
        
        initial-supply-diversity (diversity total-count)
        ]
    {:maximize 
     [- total-connection-value
      [+
       total-supply-cost
       total-pipe-cost
       emissions-cost
       total-insulation-cost
       total-alt-cost]
      ]

     :subject-to
     (list
      ;; Flow only goes one way
      (for [e edge]
        [<= [+ [:AIN (vec e)] [:AIN (reverse (vec e))]] 1])

      ;; force AIN if we use flow
      (for [a arc t period]
        [<= [:ARC-FLOW-KW a t] [* [:AIN a] [:lp.core/upper [:ARC-FLOW-KW a t]]]])

      ;; Flow balance at each vertex
      (for [i vtx t period]
        (if (and (= :mean t) (contains? dvtx i) (not= 0.0 (avoided-demand-kwh i)))
          [:<= 0 (unmet-demand i t) [:* (avoided-demand-kwh i) years-per-hour]]
          [:= 0 (unmet-demand i t)]
          ))

      ;; Constraints for arcs
      (for [a arc :let [e (as-edge a)]]
        [:and
         ;; Arcs carry their losses
         [>= [:ARC-FLOW-KW a :mean] [* [:AIN a] [:LOSS-KW e]]]

         ;; Edges have capacity for peak flow
         [>= [:EDGE-CAP-KW e] [* [:ARC-FLOW-KW a :peak] [:EDGE-DIVERSITY e]]]
         
         ;; Edges have capacity for mean flow
         [>= [:EDGE-CAP-KW e] [:ARC-FLOW-KW a :mean]]
         ])

      ;; force dvin if arc is providing heat to a building - this
      ;; means heat cannot flow through any member of dvtx or svtx
      ;; without connecting that vertex
      (for [d dvtx
            n (neighbours d)
            t period]
        (let [out  [:ARC-FLOW-KW [d n] t]
              back [:ARC-FLOW-KW [n d] t]
              is-in (if (contains? svtx d)
                      [+ [:SVIN d] [:DVIN d]]
                      [:DVIN d])]
          [:and
           [<= out  [* is-in [:lp.core/upper out]]]
           [<= back [* is-in [:lp.core/upper back]]]]))
      
      ;; supply capacity sufficient
      (for [i svtx]
        [:and
         [>= [:SUPPLY-CAP-KW i] [* [:SUPPLY-KW i :peak] [:SUPPLY-DIVERSITY i]]]
         [>= [:SUPPLY-CAP-KW i] [:SUPPLY-KW i :mean]]
         [<= [:SUPPLY-CAP-KW i] [* [:SVIN i] (supply-max-capacity i)]]
         ;; [<= [:SUPPLY-CAP-KW i] (supply-max-capacity i)] ;; redundant really
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

      ;; Required pipes
      (for [e edge :when (edge-required e)]
        [:>= [:+ [:AIN e] [:AIN (rev-edge e)]] 1]) ;; one is true

      ;; Grouped demands travel together
      (for [g grouped-demands :when (> (count g) 1)]
        [:= (for [i g] [:DVIN i])])
      
      ;; [:= [:DEBUG :pipe-cost] total-pipe-cost]
      ;; [:= [:DEBUG :supply-cost] total-supply-cost]
      ;; [:= [:DEBUG :connection-value] total-connection-value]
      ;; (for [i dvtx t period]
      ;;   [:= [:UNMET-DEMAND i t] (unmet-demand i t)
         
      ;;    ]
      ;;   )

      
      )

     
     :vars
     (cond->
         {;; debug
          ;; :UNMET-DEMAND {:indexed-by [dvtx period]}
          
          ;; :DEBUG
          ;; {:indexed-by
          ;;  [#{:connection-value
          ;;     :pipe-cost
          ;;     :supply-cost}]
          ;;  }
          
          ;; PARAMETERS (fixed = true)
          
          :EDGE-DIVERSITY
          {:indexed-by [edge] :fixed true
           :value ;; intial diversity is super-optimistic
           (fn [e]
             (diversity
              (max (:count-max (get flow-bounds e) 0)
                   (:count-max (get flow-bounds (rev-edge e)) 0))))}
          
          :SUPPLY-DIVERSITY
          {:indexed-by [svtx] :fixed true
           :value (into {}  (for [v svtx] [v initial-supply-diversity]))}
          
          :LOSS-KW
          {:indexed-by [edge] :fixed true
           :value
           (fn [e]
             (let [min-fwd  (:peak-min (get flow-bounds e) 0)
                   min-back (:peak-min (get flow-bounds (rev-edge e)) 0)
                   min-flow (min min-fwd min-back)
                   kwp      (if (zero? min-flow) (max min-fwd min-back) min-flow)]
               (edge-loss-kw-for-kwp e kwp)))}

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
          :SUPPLY-KW {:type :non-negative :indexed-by [svtx period]}}

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
     ::ins-types ins-types
     ::alt-types alt-types
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

(defn- compute-parameters
  "The mip has been solved, so we can figure out the diversity & heat
  loss parameters for the solution."
  [mip]
  (let [diversity-factor (::diversity mip) ; Get hold of diversity function

        flow-kw (-> mip :vars :ARC-FLOW-KW :value)
        ;; Find which arcs went into the solution
        arcs-in (-> mip :vars :AIN :value
                    (->> (keep (fn [[a v]] (when (and (truthy v)
                                                      (or (truthy (flow-kw [a :peak]))
                                                          (truthy (flow-kw [(rev-edge a) :peak]))
                                                          (truthy (flow-kw [a :mean]))
                                                          (truthy (flow-kw [(rev-edge a) :mean]))
                                                          ))
                                             a)))))

        
        dvin? (let [dvin (-> mip :vars :DVIN :value)]
                (fn [x] (truthy (get dvin x))))

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
        
        edge-counts                   ; the number of proper demands
                                        ; reachable through each edge
        (apply merge-with max-0
               (for [s roots] (count-up s adj #(if (dvin? %)
                                                 (-> vtx-map (get %) :demand (:count 1)) 0) +)))

        edge-max-peak-flow            ; the largest individual peak
                                        ; demand through each edge
        (apply merge-with max-0
               (for [s roots] (count-up s adj #(-> vtx-map (get %) :demand (:kwp 0)) max)))

        flow-kw (-> mip :vars :ARC-FLOW-KW :value)
        edge-loss-kw (::edge-loss mip) ;; a function, worked out before
        
        edge-parameters                 ; For edges, losses &
                                        ; diversity worked out
                                        ; together
        (->>
         (for [e (::edge mip)]
           (let [count-a (get edge-counts e)
                 count-b (get edge-counts (rev-edge e))

                 max-peak-a (get edge-max-peak-flow e)
                 max-peak-b (get edge-max-peak-flow (rev-edge e))

                 count    (or count-a count-b 0)
                 max-peak (or max-peak-a max-peak-b 0)

                 diversity (diversity-factor count)
                 undiversified-flow (max
                                     (get flow-kw [e :peak] 0)
                                     (get flow-kw [(rev-edge e) :peak] 0))

                 diversified-flow (* diversity undiversified-flow)
                 
                 diversity              ; This fixes a mistake where
                                        ; we diversify a pipe to carry
                                        ; less than the biggest load
                                        ; below it, which makes no
                                        ; sense
                 (if (and (> max-peak diversified-flow)
                          (pos? undiversified-flow))
                   (if (zero? max-peak) 1.0
                       (/ max-peak undiversified-flow))
                   diversity)

                 diversified-flow (* diversity undiversified-flow)
                 
                 heat-loss (edge-loss-kw e diversified-flow)
                 ]
             [e [diversity heat-loss]]))
         (into {}))
        ]
    {:edge-diversity    (into {} (for [[e [d]] edge-parameters] [e d]))
     :edge-losses       (into {} (for [[e [_ l]] edge-parameters] [e l]))
     :supply-diversity  (into {} (for [s (::svtx mip)]
                                   [s (diversity-factor (get edge-counts s 0.0))]))
     }))

(defn- parameterise
  "Given a MIP from construct-mip above (which may have a solution on it)
  Compute and install the values for :EDGE-DIVERSITY :SUPPLY-DIVERSITY and :LOSS-KW
  which are not determined within the program. "

  [mip]
  (if (:solution mip)
    (let [{:keys [edge-losses edge-diversity supply-diversity]} (compute-parameters mip)]
      (s/multi-transform
       [:vars
        (s/multi-path
         [:EDGE-DIVERSITY   :value (s/terminal-val edge-diversity)]
         [:SUPPLY-DIVERSITY :value (s/terminal-val supply-diversity)]
         [:LOSS-KW          :value (s/terminal-val edge-losses)])]
       mip))

    mip ;; if no solution, stick with initial parameters
    ))

(let [fix-decision (fn [var]
                     (when var
                       (assoc var
                              :fixed true
                              ::was-fixed (:fixed var))))
      unfix-decision (fn [var]
                       (when var
                         (-> var
                             (assoc :fixed (::was-fixed var))
                             (dissoc ::was-fixed))))
      ]

  (defn- fix-decisions [mip]
    (s/multi-transform
     [:vars
      (s/multi-path
       [:AIN (s/terminal fix-decision)]
       [:DVIN (s/terminal fix-decision)]
       [:SVIN (s/terminal fix-decision)]
       [(s/must :INSULATION-IN) (s/terminal fix-decision)]
       [(s/must :INSULATION-KWH) (s/terminal fix-decision)]
       [(s/must :ALT-IN) (s/terminal fix-decision)])]
     mip))
  
  (defn- unfix-decisions [mip]
    (s/multi-transform
     [:vars
      (s/multi-path
       [:AIN (s/terminal unfix-decision)]
       [:DVIN (s/terminal unfix-decision)]
       [:SVIN (s/terminal unfix-decision)]
       [(s/must :INSULATION-IN) (s/terminal unfix-decision)]
       [(s/must :INSULATION-KWH) (s/terminal unfix-decision)]
       [(s/must :ALT-IN) (s/terminal unfix-decision)])]
     mip)))

(defn- summary-decisions [mip]
  (let [vars (:vars mip)]
    (vec (for [k [:AIN :DVIN :SVIN :INSULATION-IN :INSULATION-KWH :ALT-IN]]
           (-> vars (get k) :value)))))

(defn- summary-parameters [mip]
  ;; edge diversity, supply diversity to 2dp, edge loss to nearest kw
  (when (:exists (:solution mip))
    (let [ed (-> mip :vars :EDGE-DIVERSITY :value)
          el (-> mip :vars :LOSS-KW :value)
          sd (-> mip :vars :SUPPLY-DIVERSITY :value)
          r #(/ (Math/round (* 100.0 (or % 0))) 100.0)
          ]
      [(for [e (::edge mip)]
         [e [(r (ed e)) (Math/round (el e))]])
       (for [s (::svtx mip)]
         [s (r (sd s))])])))

(defn- solve [mip & {:keys [mip-gap time-limit]}]
  (loop [attempts 0
         mip      mip
         ]

    (let [sol-free (scip/solve mip
                               :time-limit time-limit :mip-gap mip-gap
                               "numerics/feastol"  "1e-03")

          sol-par (parameterise sol-free)
          
          sol-fix (-> sol-par
                      (fix-decisions)
                      (scip/solve
                       "numerics/feastol" "1e-03")
                      (unfix-decisions))
          ]

      (cond
        (> attempts 2)
        (do
          (log/error "A feasible free solution led to an infeasible fixed solution too many times. This probably means that the supply capacity is very marginal, and the optimiser can't work out whether to include or exclude a particular demand.")
          sol-fix)

        (and (:exists (:solution sol-free)) (not (:exists (:solution sol-fix))))
        (recur (inc attempts) sol-par)
        
        :else
        (let [stable
              (= (summary-parameters sol-free)
                 (summary-parameters sol-fix))]
          ;; Copy solution information from the free version, except /value/
          ;; which is more true in the fixed one.
          (update sol-fix
                  :solution
                  merge
                  (-> (:solution sol-free)
                      (dissoc :value :exists)
                      (assoc :stable stable))))
        ))))

(defn output-solution [problem {:keys [vars solution] :as s} iters objective-values]
  (if (:exists solution)
    (let [edge           (::edge s)
          alt-types      (::alt-types s)
          ins-types      (::ins-types s)
          vtx            (into (::svtx s) (::dvtx s))
          ain            (-> vars :AIN :value)
          edge-capacity  (-> vars :EDGE-CAP-KW :value)
          edge-losses    (-> vars :LOSS-KW :value)
          edge-diversity (-> vars :EDGE-DIVERSITY :value)
          dvin           (-> vars :DVIN :value)
          svin           (-> vars :SVIN :value)
          supply-capacity (-> vars :SUPPLY-CAP-KW :value)
          supply-diversity (-> vars :SUPPLY-DIVERSITY :value)
          supply-output (-> vars :SUPPLY-KW :value)

          insulation     (-> vars :INSULATION-KWH :value)
          alternative    (-> vars :ALT-IN :value)
          has-insulation (fn [i]
                           (some #(truthy (get insulation [i %])) ins-types))
          
          has-alternative (fn [i]
                            (some #(get alternative [i %]) alt-types))
          ]
      
      (log/info "Summarising results")
      {:edges
       (for [e edge :when (or (ain e) (ain (rev-edge e)))]
         {:i           (first e)
          :j           (second e)
          :capacity-kw (edge-capacity e)
          :losses-kw   (edge-losses e)
          :diversity   (edge-diversity e)})

       :vertices
       (for [i vtx
             :let [svin (svin i)
                   dvin (dvin i)
                   has-insulation (has-insulation i)
                   has-alternative (has-alternative i)]
             :when (or svin dvin has-insulation has-alternative)]
         (merge {:id i}
                (when dvin {:connected true})
                (when svin {:capacity-kw (supply-capacity i)
                            :diversity (supply-diversity i)
                            :output-kwh (* hours-per-year (supply-output [i :mean]))})
                (when has-insulation
                  {:insulation
                   (for [t ins-types
                         :let [kwh (get insulation [i t] 0)]
                         :when (not (zero? kwh))]
                     [t kwh])})
                
                (when has-alternative
                  {:alternative (first
                                 (filter
                                  #(get alternative [i %])
                                  alt-types))})))
       
       :state     :valid
       
       :objective (:value solution)
       :solver    (assoc solution
                         :objectives objective-values
                         :iterations iters)})
    {:state (:reason solution)})
  )

(defn- human-time [msec]
  (let [sec (/ msec 1000.0)]
    (if (< sec 60)
      (str (int sec) "s")

      (let [min (/ sec 60.0)]
        (if (< min 60)
          (format "%.1fm" min)
          (let [hr (/ min 60.0)]
            (if (< hr 24)
              (format "%.1fh" hr)
              (let [d (int (/ hr 24))
                    hr (- hr (* d 24))]
                (str d "d" (int hr) "h")))))))))

(defn run-model [problem]
  (log/info "Solving network problem")
  (let [mip             (construct-mip problem)
        _               (log/info "Constructed MIP")
        iteration-limit (:iteration-limit problem 100000)
        time-limit      (:time-limit problem 1.0)
        mip-gap         (:mip-gap problem 0.05)

        start-time      (System/currentTimeMillis)
        end-time (+ (* time-limit 1000 3600) start-time)
        most-negative (- Double/MAX_VALUE)
        ]
    (log/info
     (format "%-4s%-8s%-8s%-8s%-3s%-10s%-6s%-6s%-12s"
             "N" "Tn" "T" "Tr" ">" "VALUE" "NV" "NE" "STATE"))
    
    (loop [mip      mip ;; comes parameterised out of the gate
           seen     #{} ;; decision sets we have already seen
           iters    0   ;; number of tries
           obj-vals nil ;; objective value sequence we saw
           best     nil ;; best so far
           ]
      (let [iteration-start (System/currentTimeMillis)

            solved-mip (solve mip
                              :mip-gap mip-gap
                              :time-limit
                              (max 60 (/ (- end-time iteration-start) 1000.0)))

            decisions (summary-decisions solved-mip)

            best       (if (and
                            (-> solved-mip :solution :exists)
                            (> (-> solved-mip :solution :value (or most-negative))
                               (-> best       :solution :value (or most-negative))))
                         solved-mip (or best solved-mip))

            is-stable     (:stable (:solution solved-mip))
            has-looped    (contains? seen decisions)
            out-of-iters  (> iters iteration-limit)
            iteration-end (System/currentTimeMillis)
            out-of-time   (> iteration-end end-time)
            remaining-time (- end-time iteration-end)
            ]

        (log/info (try (format "%-4d%-8s%-8s%-8s%-3s%-10.2g%-6d%-6d%-12s"
                               iters
                               (human-time (- iteration-end iteration-start))
                               (human-time (- iteration-end start-time))
                               (human-time remaining-time)
                               
                               (if (identical? best solved-mip) "*" "-")
                               (:value  (:solution solved-mip))
                               (-> solved-mip :vars :DVIN :value vals
                                   (->> (reduce (fn [n v] (cond-> n v inc)) 0)))
                               (-> solved-mip :vars :AIN  :value vals
                                   (->> (reduce (fn [n v] (cond-> n v inc)) 0)))
                               (:reason (:solution solved-mip))
                               )
                       (catch Exception e
                         "Error formatting progress row!"))
                  
                  )
        
        
        (if (or has-looped out-of-iters out-of-time is-stable)
          (do
            (when is-stable    (log/info "Solution is stable"))
            (when has-looped   (log/info "Solution is looping"))
            (when out-of-iters (log/info "Iteration limit reached"))
            (when out-of-time  (log/info "Time limit reached"))
            (log/info "Best solution:" (dissoc (:solution best) :log))
            (output-solution problem best iters obj-vals))
          (recur solved-mip (conj seen decisions) (inc iters)
                 (conj obj-vals (:value (:solution solved-mip)))
                 best)
          )))))


(comment
  (def problem (with-open [r (java.io.PushbackReader. (io/reader "/home/hinton/tmp/problem.edn"))]
                 (binding [*read-eval* false]

                   (read r))
                 ))


  (def soln (run-model problem))

  )
