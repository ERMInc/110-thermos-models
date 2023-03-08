;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos.opt.net.bounds
  "Flow bounds calculation for network model problems"
  (:require [thermos.opt.net.diversity :refer [diversity-factor]]
            [thermos.opt.net.graph :as graph]
            [clojure.tools.logging :as log]
            [clojure.set :as set]))



(defn nzmin "Minimum of x and y which is not zero" [x y]
  (cond (zero? x) y
        (zero? y) x
        :else (min x y)))

(defn- vertex-information
  "Given input vertices in network problem format (a list of maps)
  produce a summary which goes {vertex-id {fields below}}"
  [{:keys [vertices] :as problem}]
  (let [diversity (diversity-factor problem)]
    (reduce
     (fn [m v]
       (assoc m (:id v)
              (let [n (-> v (:demand {:count 0}) (:count 1))]
                {:supply-capacity-kw  (/ (-> v :supply (:capacity-kw 0)) (diversity 1000))
                 :demand-kwh          (-> v :demand (:kwh 0))
                 ;; TWEAK to undo diversity here (so we can redo it later)
                 :demand-kwp          (/ (-> v :demand (:kwp 0)) (diversity n))
                 ;; without diversity, for pipe costs
                 :demand-dkwp         (-> v :demand (:kwp 0))
                 :supply-capacity-dkw (-> v :supply (:capacity-kw 0))
                 :count               n})
              ))
     {} vertices)))


(defn- make-adjacency
  "Given input vertices from `vertex-information` and edges in network problem format (a list of edges),
  produce an adjacency map. The vertex information is used to decide
  when to exclude an arc that terminates in a demand, so that heat cannot flow through a building."
  [{:keys [edges]} vertices]

  (reduce
   (fn [adj {:keys [i j]}]
     (let [vi (get vertices i)
           vj (get vertices j)
           
           i-demand (not (zero? (:demand-kwp vi 0)))
           i-supply (not (zero? (:supply-capacity-kw vi 0)))

           j-demand (not (zero? (:demand-kwp vj 0)))
           j-supply (not (zero? (:supply-capacity-kw vj 0)))
           ]
       (cond-> adj
         ;; Does i->j?
         (or i-supply (and (not i-demand) (or j-demand (not j-supply))))
         (update i graph/sconj j)
         
         ;; Does j->i?
         (or j-supply (and (not j-demand) (or i-demand (not i-supply))))
         (update j graph/sconj i))))
   
   {} edges))

(def NOTHING
  "The bounds for an arc which cannot be included in any solution"
  {:count-min  0 :peak-min   0 :mean-min 0
   :count-max  0 :peak-max   0 :mean-max 0
   :diverse-peak-min 0 :diverse-peak-max 0})

(defn- single-edge-bounds
  "Compute the bounds for a single edge based on what it bridges.
  - `vertices` is the output of `vertex-information`
  - `upstream` is the set of vertices upstream of the edge, from which heat might flow
  - `downstream` is the set of vertices downstream of the edge, into which heat might flow
  "
  [vertices upstream downstream]
  (let [upstream-supply (transduce
                         (keep #(-> vertices (get %) :supply-capacity-kw))
                         + 0 upstream)

        d-upstream-supply (transduce
                           (keep #(-> vertices (get %) :supply-capacity-dkw))
                           + 0 upstream)
        ]

    (if (or (zero? upstream-supply) (empty? downstream)) ;; there is no edge in this direction
      NOTHING
      (loop [downstream downstream

             ;; TODO for edges that are bridges, the min can
             ;; be improved using knowledge of what is
             ;; /required/ on the other side.
             
             count-min  0
             peak-min   0
             d-peak-min 0
             mean-min   0

             count-max  0
             peak-max   0
             d-peak-max 0
             mean-max   0]
        (if (empty? downstream)
          {:count-min        count-min
           :count-max        count-max
           :peak-min         (min upstream-supply peak-min)
           :peak-max         (min upstream-supply peak-max)
           :diverse-peak-min (min d-upstream-supply d-peak-min)
           :diverse-peak-max (min d-upstream-supply d-peak-max)
           :mean-min         mean-min
           :mean-max         mean-max}

          (let [[v & downstream] downstream
                peak             (-> vertices (get v) (:demand-kwp 0))
                d-peak           (-> vertices (get v) (:demand-dkwp 0))
                mean             (-> vertices (get v) (:demand-kwh 0))
                count            (-> vertices (get v) (:count 0))]
            (recur
             downstream

             (nzmin count-min count)
             (nzmin peak-min peak)
             (nzmin d-peak-min d-peak)
             (nzmin mean-min mean)

             (+ count-max count)
             (+ peak-max peak)
             (+ d-peak-max d-peak)
             (+ mean-max mean))))))))

#_
(defn compute-bounds-old
  "Problem is a network model problem, as defined by the specs adjacent.
  
  This function should compute a structure which looks like

  {[i j] => {:count-min 0 :peak-min 0 :mean-min 0
             :count-max 0 :peak-max 0 :mean-max 0}}

  i.e. it maps from an ARC i->j to a tuple. The first thing in the
  tuple gives lower bounds, and the second gives upper bounds.

  The bounds are for the maximum flow of that type through the arc in
  that direction.

  An important tweak is that the peak flow includes un-diversification
  of demands.
  "
  [problem]
  (let [vertices (vertex-information problem)

        ;; a map that goes from VERTEX to
        ;; ADJACENT SET. It is DIRECTED, so i->j
        ;; doesn't imply j->i
        adjacency  (make-adjacency problem vertices)
        inv-adjacency (graph/invert-adjacency-map adjacency)

        set-bounds (memoize (partial single-edge-bounds vertices))
        
        arc-bounds
        (fn [[i j]]             ;; bounds to put on edge FROM i TO j
          (let [downstream
                (-> adjacency
                    (update i disj j) ;; delete edge
                    (update j disj i) ;; delete rev-edge in case
                    (graph/reachable-from #{j}))
                
                upstream
                (-> inv-adjacency
                    (update i disj j)
                    (update j disj i)
                    (graph/reachable-from #{i}))
                ]
            ;; so now we have vertices reachable from j (the far side of the edge)
            ;; and vertices from which i is reachable (the near side of the edge)
            (set-bounds upstream downstream)
            ))
        
        ]
    ;; consider using pmap here?
    (let [all-arcs (mapcat (juxt (juxt :i :j) (juxt :j :i)) (:edges problem))
          solution (into {} (map (fn [a] [a (arc-bounds a)]) all-arcs))
          ]
      (log/info "Bounds computed")
      solution
      )))

(defn pmap-n
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."

  ([n f coll]
   (let [rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([n f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn compute-bounds
  "Problem is a network model problem, as defined by the specs adjacent.
  
  This function should compute a structure which looks like

  {[i j] => {:count-min 0 :peak-min 0 :mean-min 0 :diverse-peak-min 0
             :count-max 0 :peak-max 0 :mean-max 0 :diverse-peak-max 0}}

  i.e. it tells you for the arc from i->j, if the arc is to be used,
       what are the min/max values for N connections, peak heat, avg heat,
       and diverse peak.

  An important tweak is that the peak flow includes un-diversification
  of demands by count (so there is also diverse-peak-min/max).

  The method in this one is a bit complicated compared to the more
  stupid implementation above.

  The algorithm is:

  1. Some edges are bridges in the graph; a bridge is an edge whose removal partitions the graph
  2. All other edges are inside a patch that would be connected with no bridges.
     Within a patch like that, all edges will have about the same result.
     This is because we are not going to count up all the hamiltonian paths through the patch.
     So the flows within the patch are determined by what can enter/leave that patch through
     the bridges that terminate in it
  3. Consequently, we can solve two sets of problems:
     1. What are the flow bounds on each bridge in the graph
     2. If we label all vertices by the component they are in when all bridges are gone,
        what are the flow bounds on any single edge in each component.
        We don't need to solve all the other internal edges in that component.

  As a subtle tweak, before thinking about bridges we take out every edge that is a
  connector. This is because every connector should be considered as a bridge, even
  if more than one connector terminates in a building; these are not strictly bridges
  but they are unidirectional for heat.
  "
  [problem]
  
  (let [vertices  (vertex-information problem)
        adjacency (make-adjacency problem vertices)
        inverse   (graph/invert-adjacency-map adjacency)

        connectors (set (for [j (keys vertices)
                              :when (and (zero? (:supply-capacity-kw (vertices j)))
                                         (not (zero? (:demand-kwp    (vertices j)))))
                              i (inverse j)] [i j]))

        ;; we also need the edges that can only flow out of supplies
        connectors (into connectors
                         (for [i (keys vertices)
                               :when (and (not (zero? (:supply-capacity-kw (vertices i))))
                                          (zero? (:demand-kwp    (vertices i))))
                               j (adjacency i)] [i j]))

        anti-connectors (set (for [[i j] connectors] [j i]))

        single-edge-bounds (memoize (partial single-edge-bounds vertices))
        
        ;; this is all bridges internal to the graph once there are no connectors.
        ;; needed because power cannot flow up a connector, but bridge finding
        ;; cannot see that fact.
        internal-bridges (->> (reduce
                               (fn [a [i j]]
                                 (-> a (update i disj j) (update j disj i)))
                               adjacency
                               connectors)
                              (graph/bridges)
                              (set))
        
        bridges    (concat internal-bridges connectors anti-connectors)
        parallelism (min 4 (.availableProcessors (Runtime/getRuntime)))

        bridge-bounds (->> (pmap-n
                            parallelism
                            (fn [[i j]]
                              (let [adjacency (-> adjacency (update i disj j) (update j disj i))
                                    inverse   (-> inverse (update i disj j) (update j disj i))]
                                [[[i j] (if (contains? anti-connectors [i j])
                                          NOTHING
                                          (single-edge-bounds (graph/reachable-from inverse #{i})
                                                              (graph/reachable-from adjacency #{j})))]
                                 [[j i] (if (contains? anti-connectors [j i])
                                          NOTHING
                                          (single-edge-bounds (graph/reachable-from inverse #{j})
                                                              (graph/reachable-from adjacency #{i})))]]))
                            bridges)
                           (reduce (fn [a es] (into a es)) {}))
        
        ;; a version of adjacency with all bridges removed
        components (reduce
                    (fn [a [i j]] (-> a (update i disj j) (update j disj i)))
                    adjacency bridges)

        ;; now label every vertex by which component it is in.
        component-labels (graph/label-components components)

        ;; now we want to solve for each component what bounds should pertain inside it.
        ;; if the theory is that for each such edge the answer is the same rather than thinking hard
        ;; we can just do it once and find out. Forward and reverse should be same as well?

        component-bounds (atom {})

        _ (log/info "Flow bounds for" (count internal-bridges) "internal bridges,"
                    (count connectors) "connectors,"
                    (count (set (vals component-labels))) "components,"
                    (count (:edges problem)) "edges total"
                    )


        result
        (->> (:edges problem) (mapcat (juxt (juxt :i :j) (juxt :j :i)))
             (pmap-n
              parallelism
              (fn [[i j]]
                [[i j]
                 (if-let [x (or (bridge-bounds [i j]) (@component-bounds [i j]))]
                   x
                   
                   (let [ci (component-labels i)
                         cj (or (component-labels j) ci)
                         ci (or ci cj)]
                     (assert (= ci cj) (str "Edge should be within component"
                                            " ci: " ci
                                            " cj: " cj
                                            " i: " i
                                            " j: " j))
                     (let [adjacency (-> adjacency (update i disj j) (update j disj i))
                           inverse   (-> inverse (update i disj j) (update j disj i))
                           result    (single-edge-bounds (graph/reachable-from inverse #{j})
                                                         (graph/reachable-from adjacency #{i}))]
                       (swap! component-bounds assoc (or ci cj) result)
                       result))
                   )]))
             (into {}))
        
        #_#_ old-result (compute-bounds-old problem)
        ]

    #_ (when (not= old-result result)
      (log/error "bounds differ!")
      (doseq [a (keys old-result)]
        (when-not (= (old-result a) (result a))
          (cond (and (contains? anti-connectors a) (= NOTHING (result a)))
                ;; (log/warn a "anti-connector disallowed as it should be...")
                (do)

                (= NOTHING (result a))
                
                (let [[i j] a]
                  (log/error a "path forbidden" (old-result a)
                             (contains? connectors a)
                             (contains? anti-connectors a)
                             (contains? (set bridges) a)
                             ))

                :else
                (do
                  (log/warn "difference" a)
                  (log/error "old" (if (= NOTHING (old-result a)) "NO" (old-result a)))
                  (log/error "new" (if (= NOTHING (result a)) "NO" (result a))))
                ))))

    result
    ))



(comment
  (time (let [_ (compute-bounds -last-input)]))
  (time (let [_ (compute-bounds-old -last-input)]))
  )
