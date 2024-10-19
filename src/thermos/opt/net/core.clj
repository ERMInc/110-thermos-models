;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.core
  "Thermos network optimisation model. Translated from the python version."
  (:require [lp.scip :as scip]
            [lp.gurobi :as gurobi]
            [com.rpl.specter :as s]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [thermos.opt.net.specs :refer [network-problem]]
            [clojure.spec.alpha :as spec]
            [thermos.opt.net.diversity :refer [diversity-factor]]
            [thermos.opt.net.bounds :as bounds]
            [thermos.opt.net.graph :as graph]
            [clojure.set :as set]))

(let [env (into {} (System/getenv))]
  (def initial-feastol (or (env "THERMOS_INITIAL_FEASTOL") "1e-3"))
  (def retry-feastol   (or (env "THERMOS_RETRY_FEASTOL")   "1e-6")))

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

(defn- interior-edge-fn
  "Given the :edges from a problem, return a function
  which will find the edges interior to a group of vertices.

  This is a set of edges which must all be used if the vertices
  are part of a group and the vertices are connected, so for
  example all the connectors for that group.

  In practice this implements a single case which is this:
  
        a    b   c
        |    |   |
  *1----2----3---4---5

  In which it should return the two mid-parts.

  The connectors will be tied to the group by a different
  set of constraints.
  "
  [edges]
  (let [adj (persistent!
             (reduce
              (fn [a {:keys [i j]}]
                (assoc! a
                        i (conj (get a i #{}) j)
                        j (conj (get a j #{}) i)))
              (transient {})
              edges))]
    (fn interior-edges [vertices]
      (let [midpoints (map adj vertices)]
        (when (= #{1} (set (map count midpoints)))
          (let [midpoints (reduce into #{} midpoints)
                edges (for [m midpoints
                            n (adj m)
                            :when (contains? midpoints n)
                            ;; so we only get edges not arcs
                            :when (neg? (compare m n))]
                        [m n])]
            ;; check edges forms a line?
            (when (= midpoints (set (flatten edges)))
              edges)
            ))))))

(comment
  (let [ief (interior-edge-fn
             [{:i 1 :j 2}
              {:i 2 :j 3}
              {:i 3 :j 4}
              {:i 4 :j 5}
              {:i 2 :j :a}
              {:i 3 :j :b}
              {:i 4 :j :c}])]
    (ief #{:a :b :c})))

(defn- edges-to-single-vertices
  "Return a sequence of [[i j] k] tuples, where edge [i j] is in arc and
  k is a member of dvtx. The edges returned are those which can only reach
  the given vertex k, when starting at members of svtx"
  [arc svtx dvtx]
  (let [adj (reduce
             (fn [a [i j]] (assoc a i (conj (get a i #{}) j)))
             {}
             arc)
        rt (graph/reachable-through adj svtx)]
    ;; rt maps edges to sets of vertices reachable through edges
    (for [[e ks] rt
          :let [ks (set/intersection dvtx ks)]
          :when (= 1 (count ks))]
      [(as-edge e) (first ks)])))


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

        interior-edges (interior-edge-fn (:edges problem))
        
        alt-types  (set (mapcat (comp keys :alternatives :demand) (:vertices problem)))
        ins-types  (set (mapcat (comp keys :insulation :demand)   (:vertices problem)))

        ;; constants
        min-cap-cost (:minimum-capacity-cost problem 0.01)
        flow-bound-slack (:flow-bound-slack problem 1.5)

        vertices   (assoc-by :id (:vertices problem))
        emissions  (:emissions problem)
        arc-map    (merge (assoc-by (comp vec (juxt :i :j)) (:edges problem))
                          (assoc-by (comp vec (juxt :j :i)) (:edges problem)))
        
        ;; accessor functions
        demand-kwp (fn [i] (or (-> vertices (get i) :demand :kwp) 0))
        demand-kwh (fn [i] (or (-> vertices (get i) :demand :kwh) 0))

        demand-connection-value
        (fn [i] (boolean (-> vertices (get i) :demand :required)))

        demand-connection-fixed
        (fn [i] (or (boolean (-> vertices (get i) :demand :required))
                    (boolean (-> vertices (get i) :demand :off-network))))

        edge-fixed-cost
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (get e :cost%m 0)) 0)))

        edge-cost-per-kwp
        (fn [e] (let [e (get arc-map e)]
                  (if e (* (:length e 0) (get e :cost%kwm 0)) 0)))

        neighbours          (into {} (for [[i ijs] (group-by first arc)]
                                        [i (set (map second ijs))]))

        ;; we can restrict supply capacity a bit using the flow bounds
        ;; which can't hurt model efficiency. We use the diversified
        ;; value but when using it as a big M on undiversified flow we
        ;; divide by max diversity to slacken it off a bit.
        supply-max-capacity
        (let [supply-bounds
              (reduce
               (fn [a k]
                 (let [summed-flow-bounds
                       (reduce + (demand-kwp k)
                               (for [n (neighbours k)]
                                 (-> flow-bounds (get [k n]) :diverse-peak-max (or 0))))
                       flow-bound (* flow-bound-slack summed-flow-bounds)
                       given-capacity (-> (vertices k) :supply :capacity-kw (or 0))
                       ]
                   (assoc a k (min flow-bound given-capacity))))
               {} svtx)]

          (fn [i] (or (get supply-bounds i) 0)))

        supply-max-mean
        (let [supply-bounds
              (reduce
               (fn [a k]
                 (let [summed-flow-bounds
                       (reduce + (demand-kwp k)
                               (for [n (neighbours k)]
                                 (-> flow-bounds (get [k n]) :mean-max (or 0))))
                       flow-bound (* flow-bound-slack summed-flow-bounds)
                       given-capacity (some-> (vertices k) :supply :capacity-kwh
                                              (* years-per-hour))
                       
                       ]
                   (assoc a k (min flow-bound (or given-capacity flow-bound)))))
               {} svtx)]

          (fn [i] (get supply-bounds i)))
        
        supply-fixed-cost   (fn [i] (or (-> (vertices i) :supply :cost) 0))
        supply-cost-per-kwh (fn [i] (or (-> (vertices i) :supply (get :cost%kwh)) 0))
        supply-cost-per-kwp (fn [i] (or (-> (vertices i) :supply (get :cost%kwp)) 0))
        supply-emissions-per-kw (fn [i e]
                                  (* (or (-> (vertices i) :supply :emissions (get e)) 0)
                                     hours-per-year))
        supply-count-max         (:supply-limit problem)
        

        vertex-fixed-value   (fn [i] (or (-> (vertices i) :demand :value) 0))
        vertex-value-per-kwp (fn [i] (or (-> (vertices i) :demand (get :value%kwp)) 0))
        vertex-value-per-kwh (fn [i] (or (-> (vertices i) :demand (get :value%kwh)) 0))
        vertex-alternatives  (fn [i]
                               (-> (vertices i) :demand :alternatives keys set))

        vertex-demand-count  (fn [i] (if (dvtx i)
                                       (-> (vertices i) :demand (:count 1))
                                       0))
        
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
          [:+
           (for [i svtx] [:* [:SUPPLY-KW i :mean] (supply-emissions-per-kw i e)])
           (for [i dvtx a alt-types :let [f (alternative-emissions-per-kwh i a e)]]
             [:-
              [:* [:ALT-IN i a] (demand-kwh i) f]
              [:* [:ALT-AVOIDED-KWH i a] f]])])

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
        [:+ (for [i dvtx]
             [:-
              [:*
               [:DVIN i]
               (+ (vertex-fixed-value i)
                  (* (demand-kwh i) (vertex-value-per-kwh i))
                  (* (demand-kwp i) (vertex-value-per-kwp i)))]

              ;; we don't get paid for unmet demand
              ;; we don't want to multiply it by DVIN though, since that's quadratic
              [:* (unmet-demand i :mean)
                 hours-per-year
                 (vertex-value-per-kwh i)]])]

        total-supply-cost
        [:+ (for [i svtx]
             [:+
              [:* [:SVIN i] (supply-fixed-cost i)]
              [:* [:SUPPLY-CAP-KW i] (max min-cap-cost (supply-cost-per-kwp i))]
              [:* [:SUPPLY-KW i :mean] (max min-cap-cost (supply-cost-per-kwh i)) hours-per-year]])]

        total-pipe-cost
        [:+ (for [e edge :let [[i j] e]]
             [:+
              [:* [:+ [:AIN [i j]] [:AIN [j i]]] (edge-fixed-cost e)]
              [:* [:EDGE-CAP-KW e] (max min-cap-cost (edge-cost-per-kwp e))]])]
        
        emissions-cost
        [:+ (for [e emission]
             [:*
              (emissions-cost-per-kg e)
              (total-emissions e)])]

        total-insulation-cost
        [:+ (for [i dvtx it ins-types]
             [:+
              [:* [:INSULATION-IN i it] (insulation-fixed-cost i it)]
              [:* [:INSULATION-KWH i it] (insulation-cost-per-kwh i it)]])]

        total-alt-cost
        [:+ (for [i dvtx a alt-types]
             [:-
              [:* [:ALT-IN i a]
               (+ (alternative-fixed-cost i a)
                  (* (alternative-cost-per-kwp i a) (demand-kwp i))
                  (* (alternative-cost-per-kwh i a) (demand-kwh i)))]

              ;; don't pay for what we didn't use due to insulation
              [:* [:ALT-AVOIDED-KWH i a] (alternative-cost-per-kwh i a)]])]

        avoided-demand-kwh
        (->> (for [i dvtx]
               [i (if (seq ins-types)
                    [:+ (for [it ins-types] [:INSULATION-KWH i it])]
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

        exclusive-supply-groups
        (reduce
         (fn [a vid]
           (->> (-> vertices (get vid) :supply :exclusive-groups)
                (reduce (fn [a g] (update a g conj vid)) a)))
         {}
         svtx)
        ]
    {:maximize 
     [:- total-connection-value
      [:+
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
        [:<= [:+ [:AIN (vec e)] [:AIN (reverse (vec e))]] 1])

      ;; force AIN if we use flow
      (for [a arc t period]
        [:<= [:ARC-FLOW-KW a t] [:* [:AIN a] [:lp.core/upper [:ARC-FLOW-KW a t]]]])

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
         [:>= [:ARC-FLOW-KW a :mean] [:* [:AIN a] [:LOSS-KW e]]]

         ;; Edges have capacity for peak flow
         [:>= [:EDGE-CAP-KW e] [:* [:ARC-FLOW-KW a :peak] [:EDGE-DIVERSITY e]]]
         
         ;; Edges have capacity for mean flow
         [:>= [:EDGE-CAP-KW e] [:ARC-FLOW-KW a :mean]]
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
                      [:+ [:SVIN d] [:DVIN d]]
                      [:DVIN d])]
          [:and
           [:<= out  [:* is-in [:lp.core/upper out]]]
           [:<= back [:* is-in [:lp.core/upper back]]]]))

      ;; we also wish to say that if you build a pipe it should carry some peak flow.
      ;; this prevents the optimiser building pipes to nowhere, which can otherwise
      ;; happen if they don't cost much
      (for [a arc] [:<= [:AIN a] [:ARC-FLOW-KW a :peak]])
      
      ;; supply capacity sufficient
      (for [i svtx
            :let [max-diverse-peak (supply-max-capacity i)
                  max-raw-peak     (/ max-diverse-peak (diversity 1000.0))
                  max-mean         (or (supply-max-mean i) max-raw-peak)]]
        [:and
         [:>= [:SUPPLY-CAP-KW i]   [:* [:SUPPLY-KW i :peak] [:SUPPLY-DIVERSITY i]]]
         [:>= [:SUPPLY-CAP-KW i]   [:SUPPLY-KW i :mean]]
         [:<= [:SUPPLY-CAP-KW i]   [:* [:SVIN i] max-diverse-peak]]
         ;; TODO these two _should_ be redundant but putting them in
         ;; strengthens the bounds and makes model work better when
         ;; supply-max-capacity is very large.
         [:<= [:SUPPLY-KW i :peak] [:* [:SVIN i] max-raw-peak]]
         [:<= [:SUPPLY-KW i :mean] [:* [:SVIN i] max-raw-peak]]
         [:<= [:SUPPLY-KW i :mean] [:* [:SVIN i] max-mean]]
         
         [:<= [:SVIN i]            [:SUPPLY-KW i :peak]]])
      
      ;; not too many supplies
      (when supply-count-max
        [:<= [:+ (for [i svtx] [:SVIN i])] supply-count-max])

      ;; only one supply from each exclusive supply group
      ;; the :when is (seq (rest vs)) because an exclusive group
      ;; of 1 doesn't require a constraint
      (for [[_ vs] exclusive-supply-groups :when (seq (rest vs))]
        [:<= [:+ (for [i vs] [:SVIN i])] 1])

      ;; emissions limits = 
      (for [e emission
            :let [lim (emissions-limit e)] :when lim]
        [:<= (total-emissions e) lim])

      ;; rules for alternatives
      ;; 1. we must pick a heating system
      (for [i dvtx
            :when (not-empty (vertex-alternatives i))]
        [:= 1 [:+
              [:DVIN i]
              (for [a alt-types] [:ALT-IN i a])]])


      (for [i dvtx a alt-types]
        [:and
         ;; 2. We can avoid as much as insulation allows us to
         [:<= [:ALT-AVOIDED-KWH i a] (avoided-demand-kwh i)]
         ;; 3. But only if we are actually using this alt.
         [:<= [:ALT-AVOIDED-KWH i a] [:* [:ALT-IN i a] (demand-kwh i)]]])


      ;; rules for insulation:
      ;; 1. Big-M constraint to toggle payment of fixed cost.
      (for [i dvtx it ins-types]
        [:<= [:INSULATION-KWH i it] [:* (insulation-max-kwh i it) [:INSULATION-IN i it]]])

      ;; Required pipes
      (for [e edge :when (edge-required e)]
        [:>= [:+ [:AIN e] [:AIN (rev-edge e)]] 1]) ;; one is true

      ;; Grouped demands travel together
      (for [g grouped-demands :when (> (count g) 1)]
        [:= (for [i g] [:DVIN i])])

      ;; interior edges to groups travel together
      (for [g grouped-demands :when (> (count g) 1)]
        [:= (for [e (interior-edges g)] [:+ [:AIN e] [:AIN (rev-edge e)]])])

      ;; interior edges to groups are on if groups are on
      (for [g grouped-demands :when (> (count g) 1)
            v g]
        [:>=
         (for [e (interior-edges g)]
           [:+ [:AIN e] [:AIN (rev-edge e)]])
         [:DVIN v]])

      ;; paths that reach only to a single vertex are off if the vertex is off
      ;; note that they are not on if the vertex is on, because it might be a diamond.
      (for [[edge demand] (edges-to-single-vertices arc svtx dvtx)
            :when (not (contains? svtx demand))]
        [:<=
         [:+ [:AIN edge] [:AIN (rev-edge edge)]]
         [:DVIN demand]])

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
                 :value demand-connection-value
                 :fixed demand-connection-fixed}
          
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
        (let [force-insulation (:force-insulation problem)
              insulation-fixed (fn [i it]
                                    (or (not (insulation-allowed i it)) force-insulation))
              ]
          {:INSULATION-KWH {:type :non-negative :indexed-by [dvtx ins-types]
                            :lower insulation-min-kwh
                            :upper insulation-max-kwh
                            :value (fn [i it]
                                     (cond
                                       (not (insulation-allowed i it)) 0
                                       force-insulation                (insulation-max-kwh i it)))
                            :fixed insulation-fixed}
           :INSULATION-IN {:type :binary :indexed-by [dvtx ins-types]
                           :value (fn [i it]
                                    (cond
                                      (not (insulation-allowed i it)) false
                                      force-insulation                true))
                           :fixed insulation-fixed}})))
     
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

(defn- truthy [v] (or (= true v) (and (number? v) (> v 0.5))))

(defn- compute-parameters
  "The mip has been solved, so we can figure out the diversity & heat
  loss parameters for the solution."
  [mip]
  (let [diversity-factor (::diversity mip) ; Get hold of diversity function

        diversity-limit (diversity-factor 100000)
        
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


        arcs-in (set arcs-in)
        
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

        vtx-map (::vtx-map mip)

        reachability (graph/close-adjacency-map adj roots)

        count-from-vertex
        (let [vcount #(if (dvin? %)
                        (-> vtx-map (get %) :demand (:count 1)) 0)
              result (reduce-kv
                      (fn [a v vs]
                        ;; v is a vertex and vs is all below verts.
                        ;; so we want to output v : on any arc /into/ v
                        (assoc a v (reduce + (vcount v) (map vcount vs))))
                      {} reachability)
              ]
          (fn [x]
            (or (result x)
                (vcount x) ;; for terminal vertices
                0)))

        max-peak-from-vertex
        (let [vpeak #(if (dvin? %)
                       (-> vtx-map (get %) :demand (:kwp 0)) 0)
              result (reduce-kv
                      (fn [a v vs]
                        ;; v is a vertex and vs is all below verts.
                        ;; so we want to output v : on any arc /into/ v
                        (assoc a v (reduce max (vpeak v) (map vpeak vs))))
                      {} reachability)
              ]
          (fn [x] (or (result x)
                      (vpeak x)
                      0)))
        
        flow-kw (-> mip :vars :ARC-FLOW-KW :value)
        edge-loss-kw (::edge-loss mip) ;; a function, worked out before

        n-diamonds (volatile! 0)
        
        edge-parameters                 ; For edges, losses &
                                        ; diversity worked out
                                        ; together
        (->>
         (for [e (::edge mip)]
           (let [a (or (arcs-in e) (arcs-in (rev-edge e))) ;; we should only have one arc in per edge
                 
                 count    (count-from-vertex (second a))
                 max-peak (max-peak-from-vertex (second a))

                 diversity (diversity-factor count)
                 undiversified-flow (get flow-kw [a :peak] 0)
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

                 diamond? (not (<= diversity-limit diversity 1.001))
                 
                 diversity (max diversity-limit (min diversity 1.0))
                 
                 diversified-flow (* diversity undiversified-flow)
                 
                 heat-loss (edge-loss-kw e diversified-flow)
                 ]
             (when diamond? (vswap! n-diamonds inc))
             [e [diversity heat-loss]]))
         (into {}))
        ]

    (when (pos? @n-diamonds)
      (log/warn @n-diamonds "edges involved in a diamond - diversity estimation simplified for these"))
    
    {:edge-diversity    (into {} (for [[e [d]] edge-parameters] [e d]))
     :edge-losses       (into {} (for [[e [_ l]] edge-parameters] [e l]))
     :supply-diversity  (into {} (for [s (::svtx mip)]
                                   [s (diversity-factor (count-from-vertex s))]))
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

(def scip-settings
  {"heuristics/actconsdiving/freq" "20"
   "heuristics/adaptivediving/freq" "3"
   "heuristics/adaptivediving/maxlpiterquot" "0.15"
   "heuristics/bound/freq" "20"
   "heuristics/clique/freq" "20"
   "heuristics/coefdiving/freq" "20"
   "heuristics/completesol/freq" "20"
   "heuristics/conflictdiving/freq" "5"
   "heuristics/conflictdiving/maxlpiterquot" "0.225"
   "heuristics/conflictdiving/maxlpiterofs" "1500"
   "heuristics/crossover/freq" "10"
   "heuristics/crossover/nwaitingnodes" "20"
   "heuristics/crossover/nodesquot" "0.15"
   "heuristics/crossover/minfixingrate" "0.5"
   "heuristics/crossover/dontwaitatroot" "TRUE"
   "heuristics/dins/freq" "-1"
   "heuristics/distributiondiving/freq" "5"
   "heuristics/distributiondiving/maxlpiterquot" "0.075"
   "heuristics/distributiondiving/maxlpiterofs" "1500"
   "heuristics/farkasdiving/freq" "-1"
   "heuristics/farkasdiving/maxlpiterquot" "0.075"
   "heuristics/farkasdiving/maxlpiterofs" "1500"
   "heuristics/feaspump/freq" "10"
   "heuristics/feaspump/maxlpiterquot" "0.015"
   "heuristics/feaspump/maxlpiterofs" "1500"
   "heuristics/fixandinfer/freq" "20"
   "heuristics/fracdiving/freq" "5"
   "heuristics/fracdiving/maxlpiterquot" "0.075"
   "heuristics/fracdiving/maxlpiterofs" "1500"
   "heuristics/gins/freq" "10"
   "heuristics/guideddiving/freq" "5"
   "heuristics/guideddiving/maxlpiterquot" "0.075"
   "heuristics/guideddiving/maxlpiterofs" "1500"
   "heuristics/zeroobj/freq" "20"
   "heuristics/intdiving/freq" "20"
   "heuristics/intshifting/freq" "5"
   "heuristics/linesearchdiving/freq" "5"
   "heuristics/linesearchdiving/maxlpiterquot" "0.075"
   "heuristics/linesearchdiving/maxlpiterofs" "1500"
   "heuristics/localbranching/freq" "-1"
   "heuristics/locks/freq" "20"
   "heuristics/lpface/freq" "8"
   "heuristics/alns/freq" "10"
   "heuristics/alns/trustregion/active" "TRUE"
   "heuristics/alns/nodesofs" "2000"
   "heuristics/alns/nodesquot" "0.2"
   "heuristics/nlpdiving/freq" "5"
   "heuristics/mutation/freq" "20"
   "heuristics/multistart/freq" "20"
   "heuristics/mpec/freq" "25"
   "heuristics/objpscostdiving/freq" "10"
   "heuristics/objpscostdiving/maxlpiterquot" "0.015"
   "heuristics/objpscostdiving/maxlpiterofs" "1500"
   "heuristics/octane/freq" "20"
   "heuristics/ofins/freq" "20"
   "heuristics/padm/freq" "20"
   "heuristics/proximity/freq" "20"
   "heuristics/pscostdiving/freq" "5"
   "heuristics/pscostdiving/maxlpiterquot" "0.075"
   "heuristics/pscostdiving/maxlpiterofs" "1500"
   "heuristics/randrounding/freq" "10"
   "heuristics/rens/freq" "20"
   "heuristics/rens/minfixingrate" "0.3"
   "heuristics/rens/nodesofs" "2000"
   "heuristics/reoptsols/freq" "20"
   "heuristics/repair/freq" "20"
   "heuristics/rins/freq" "10"
   "heuristics/rootsoldiving/freq" "10"
   "heuristics/rootsoldiving/maxlpiterquot" "0.015"
   "heuristics/rootsoldiving/maxlpiterofs" "1500"
   "heuristics/shiftandpropagate/freq" "20"
   "heuristics/shifting/freq" "5"
   "heuristics/trivial/freq" "20"
   "heuristics/trivialnegation/freq" "20"
   "heuristics/trustregion/freq" "-1"
   "heuristics/twoopt/freq" "20"
   "heuristics/undercover/freq" "20"
   "heuristics/vbounds/freq" "20"
   "heuristics/veclendiving/freq" "5"
   "heuristics/veclendiving/maxlpiterquot" "0.075"
   "heuristics/veclendiving/maxlpiterofs" "1500"
   "separating/flowcover/freq" "8"})

(defn- solve [mip & {:keys [mip-gap time-limit adjust-feastol solver]
                     :or {solver :scip}}]
  (let [run-solver (case solver
                     :scip (fn [lp s] (scip/solve* lp (merge scip-settings s)))
                     :gurobi gurobi/solve*)]
    (loop [attempts 0
           mip      mip
           feastol  initial-feastol]
      (let [sol-free (run-solver mip {:feasibility-tolerance feastol
                                      :time-limit time-limit
                                      :mip-gap mip-gap})

            sol-par (parameterise sol-free)
            
            sol-fix (if (:exists (:solution sol-free))
                      (-> sol-par
                          (fix-decisions)
                          (run-solver {})
                          (unfix-decisions))
                      sol-free)
            ]
        (cond
          (and (not (:exists (:solution sol-free)))
               (= :infeasible (:reason (:solution sol-free)))
               adjust-feastol
               (= feastol initial-feastol))
          (do
            (log/warnf "Unexpectedly infeasible (tol=%s); retry tol=%s"
                       feastol retry-feastol)
            
            (recur attempts mip retry-feastol))
          
          (> attempts 2)
          (do
            (log/error "A feasible free solution led to an infeasible fixed solution too many times. This probably means that the supply capacity is very marginal, and the optimiser can't work out whether to include or exclude a particular demand.")

            (comment
              (doseq [u (scip/minuc (fix-decisions sol-par))]
                (log/info "UC:" u))
              
              (let [svtx (::svtx sol-par)]
                (doseq [s svtx]
                  (println s "\t" "SVIN\t" (-> sol-free :vars :SVIN :value (get s)))
                  (println s "\t" "KWP\t"  (-> sol-free :vars :SUPPLY-KW :value (get [s :peak])))
                  (println s "\t" "KWA\t"  (-> sol-free :vars :SUPPLY-KW :value (get [s :mean])))
                  (println s "\t" "CAP\t"  (-> sol-free :vars :SUPPLY-CAP-KW :value (get s)))
                  (println s "\t" "D0\t"   (-> sol-free :vars :SUPPLY-DIVERSITY :value (get s)))
                  (println s "\t" "D1\t"   (-> sol-par  :vars :SUPPLY-DIVERSITY :value (get s))))))
            
            sol-fix)

          (and (:exists (:solution sol-free)) (not (:exists (:solution sol-fix))))
          (recur (inc attempts) sol-par retry-feastol)
          
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
                        (assoc :stable stable)
                        (assoc :free-value (:value (:solution sol-free)))))))
        ))))

(defn output-solution [{:keys [vars solution] :as s} iters objective-values]
  (if (:exists solution)
    (let [edge             (::edge s)
          alt-types        (::alt-types s)
          ins-types        (::ins-types s)
          vtx              (into (::svtx s) (::dvtx s))
          ain              (->> vars :AIN              :value (into {}))
          edge-capacity    (->> vars :EDGE-CAP-KW      :value (into {}))
          edge-losses      (->> vars :LOSS-KW          :value (into {}))
          edge-diversity   (->> vars :EDGE-DIVERSITY   :value (into {}))
          dvin             (->> vars :DVIN             :value (into {}))
          svin             (->> vars :SVIN             :value (into {}))
          supply-capacity  (->> vars :SUPPLY-CAP-KW    :value (into {}))
          supply-diversity (->> vars :SUPPLY-DIVERSITY :value (into {}))
          supply-output    (->> vars :SUPPLY-KW        :value (into {}))

          insulation     (-> vars :INSULATION-KWH      :value (into {}))
          alternative    (-> vars :ALT-IN              :value (into {}))
          has-insulation (fn [i]
                           (some #(truthy (get insulation [i %])) ins-types))
          
          has-alternative (fn [i]
                            (some #(get alternative [i %]) alt-types))
          ]
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
                            :output-kwh (* hours-per-year (supply-output [i :mean] 0))})
                (when has-insulation
                  {:insulation
                   (for [t ins-types
                         :let [kwh (get insulation [i t] 0)]
                         :when (and (number? kwh) (not (zero? kwh)))]
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
    {:state (:reason solution)
     :error (:error solution)}))


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

(defn run-model [problem & {:keys [solver]
                            :or {solver :scip}}]
  {:pre [(#{:scip :gurobi} solver)]}
  (log/info "Solving network problem")
  (let [mip             (construct-mip problem)
        _               (log/info "Constructed MIP")
        iteration-limit    (:iteration-limit problem 1000)
        time-limit         (:time-limit problem 1.0)
        mip-gap            (:mip-gap problem 0.05)
        param-fix-gap      (:param-gap problem 0)
        should-be-feasible (:should-be-feasible problem false)
        
        start-time      (System/currentTimeMillis)
        end-time (+ (* time-limit 1000 3600) start-time)
        most-negative (- Double/MAX_VALUE)
        ]
    (log/info
     (format "%-4s%-8s%-8s%-8s%-3s%-10s%-6s%-6s%-12s%-7s%-7s"
             "N" "Tn" "T" "Tr" ">" "VALUE" "NV" "NE" "STATE" "δFIX%" "GAP%"))

    (def -last-problem problem)
    
    
    (loop [mip      mip ;; comes parameterised out of the gate
           seen     #{} ;; decision sets we have already seen
           iters    0   ;; number of tries
           obj-vals nil ;; objective value sequence we saw
           best     nil ;; best so far
           ]
      (let [iteration-start (System/currentTimeMillis)

            solved-mip (solve mip
                              :solver solver
                              :adjust-feastol should-be-feasible
                              :mip-gap mip-gap
                              :time-limit
                              (max 60 (/ (- end-time iteration-start) 1000.0)))

            decisions (summary-decisions solved-mip)

            solution-exists (-> solved-mip :solution :exists)
            
            best       (if (and
                            solution-exists
                            (> (-> solved-mip :solution :value (or most-negative))
                               (-> best       :solution :value (or most-negative))))
                         solved-mip (or best solved-mip))

            is-stable     (:stable (:solution solved-mip))
            has-looped    (contains? seen decisions)
            out-of-iters  (> iters iteration-limit)
            iteration-end (System/currentTimeMillis)
            out-of-time   (> iteration-end end-time)
            remaining-time (- end-time iteration-end)
            parameter-delta (when solution-exists
                              (let [{:keys [value free-value]} (:solution solved-mip)]
                                (when (and value free-value)
                                  (if (= value free-value) 0
                                      (/ (- value free-value)
                                         (inc (Math/abs (max value free-value))))))))
            param-effect-small
            (and solution-exists parameter-delta
                 (<= (Math/abs parameter-delta) param-fix-gap))
            ]

        (log/info (try (format "%-4d%-8s%-8s%-8s%-3s%-10.2g%-6d%-6d%-12s%-7.1g%-7.2g"
                               iters
                               (human-time (- iteration-end iteration-start))
                               (human-time (- iteration-end start-time))
                               (human-time remaining-time)
                               
                               (if (identical? best solved-mip) "*" "-")
                               (or (:value  (:solution solved-mip)) Double/NaN)

                               (try (if solution-exists
                                      (-> solved-mip :vars :DVIN :value vals
                                          (->> (reduce (fn [n v] (cond-> n v inc)) 0)))
                                      -1)
                                    (catch Exception e -1))

                               (try (if solution-exists
                                      (-> solved-mip :vars :AIN  :value vals
                                          (->> (reduce (fn [n v] (cond-> n v inc)) 0)))
                                      -1)
                                    (catch Exception e -1))

                               (:reason (:solution solved-mip))
                               (* (or parameter-delta Double/NaN) 100.0)
                               (* (or (:gap (:solution solved-mip)) Double/NaN) 100.0))
                       
                       (catch Exception e
                         (log/error e "Unable to format row in table")
                         (str (dissoc (:solution solved-mip) :log)))))
        
        (if (or has-looped out-of-iters out-of-time is-stable param-effect-small)
          (do
            (when param-effect-small (log/info "Parameter fixing has small effect") )
            (when is-stable    (log/info "Solution is stable"))
            (when has-looped   (log/info "Solution is looping"))
            (when out-of-iters (log/info "Iteration limit reached"))
            (when out-of-time  (log/info "Time limit reached"))
            (log/info "Best solution:" (dissoc (:solution best) :log))
            (output-solution best iters obj-vals))
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

  (def junk (output-solution nil -last-solution nil nil ))

  (def mip (construct-mip -last-problem))
  (run-model (assoc -last-problem :flow-bound-slack 2))

  (scip/minuc mip)
  (def b (bounds/compute-bounds -last-problem))

  (with-open [w (io/writer "/home/hinton/tmp/main.dot")]
    (binding [*out* w]
      (println "digraph G {")
      (doseq [[[a b] {:keys [peak-max]}] (:bounds -last-problem)]
        (when-not (zero? peak-max)
          (println (format "\"%s\" -> \"%s\" [label=\"%.3g\"];" a b peak-max))
          )
        )
      (println "}")
      )
    )
  
  
  (def soln
    (binding [lp.io/*keep-temp-dir* true]
      (run-model -last-problem)
      ))
  )
