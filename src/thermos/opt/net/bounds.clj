(ns thermos.opt.net.bounds
  "Flow bounds calculation for network model problems"
  (:require [thermos.opt.net.diversity :refer [diversity-factor]]
            [clojure.tools.logging :as log]))

(defn reachable-from
  "Given `adj` which maps indices to sets of indices, and `js`, a
  starting set of indices, return the set of all indices reachable
  from `js` through `adj`

  For example

  (reachable-from {1 #{2} 2 #{3}} #{1}) => #{1 2 3}
  (reachable-from {1 #{2} 2 #{3}} #{2}) => #{2 3}
  "
  [adj js]
  
  ;; TODO perf: could use transient sets
  (let [adj (mapcat adj)]
    (loop [js js]
      (let [js' (into js adj js)]
        (if (= js' js) js (recur js' ))))))

(defn sconj
  "Conj x with set s, producing a set"
  [s x] (conj (or s #{}) x))

(defn invert-adjacency-map
  "Given a map in which a key points to a set of successor keys,
  return another map in which each successor points to a set of keys
  that pointed to it in the input. Function should be its own inverse
  
  For example
  (invert-adjacency-map {1 #{2 3} 4 #{2 6}}) => {2 #{1 4} 3 #{1} 6 #{4}}
  "
  [adjacency]
  (persistent!
   (reduce-kv
    (fn [iadj k vs]
      (reduce
       (fn [iadj v]
         (assoc! iadj v (sconj (get iadj v) k)))
       iadj vs))
    (transient {}) adjacency)))

(defn nzmin "Minimum of x and y which is not zero" [x y]
  (cond (zero? x) y
        (zero? y) x
        :else (min x y)))

(comment
  ;; for example:
  (=
   (invert-adjacency-map
    {1 #{2 3} 4 #{2 6}})
   {2 #{1 4} 3 #{1} 6 #{4}}))

(defn compute-bounds
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
  [{:keys [vertices edges] :as problem}]
  (log/info "Compute flow bounds for" (count edges) "edges")
  (let [diversity (diversity-factor problem)
        
        vertices ;; arrange vertices by id, keep salient facts
        (reduce
         (fn [m v]
           (assoc m (:id v)
                  (let [n (-> v (:demand {:count 0}) (:count 1))]
                    {:supply-capacity-kw      (/ (-> v :supply (:capacity-kw 0)) (diversity 1000))
                     :demand-kwh              (-> v :demand (:kwh 0))
                     ;; TWEAK to undo diversity here (so we can redo it later)
                     :demand-kwp              (/ (-> v :demand (:kwp 0)) (diversity n))
                     ;; without diversity, for pipe costs
                     :demand-dkwp  (-> v :demand (:kwp 0))
                     :supply-capacity-dkw (-> v :supply (:capacity-kw 0))
                     :count                   n}
                    )
                  
                  ))
         {} vertices)

        adjacency ;; a map that goes from VERTEX to
        ;; ADJACENT SET. It is DIRECTED, so i->j
        ;; doesn't imply j->i
        (reduce
         (fn [adj {:keys [i j]}]
           (let [vi (get vertices i)
                 vj (get vertices j)
                 
                 i-demand (not (zero? (:demand-kwp vi 0)))
                 i-supply (not (zero? (:supply-capacity-kw vi 0)))

                 j-demand (not (zero? (:demand-kwp vj 0)))
                 j-supply (not (zero? (:supply-capacity-kw vj 0)))
                 ]

             ;; TODO This includes some edges that are not needed,
             ;; from a pure junction to a supply.
             (cond-> adj
               (or i-supply (not i-demand))
               (update i sconj j)

               (or j-supply (not j-demand))
               (update j sconj i))))
         
         {} edges)

        NOTHING {:count-min  0 :peak-min   0 :mean-min 0
                 :count-max  0 :peak-max   0 :mean-max 0
                 :diverse-peak-min 0 :diverse-peak-max 0
                 }
        
        inv-adjacency (invert-adjacency-map adjacency)

        set-bounds
        (fn [upstream downstream]
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

        set-bounds (memoize set-bounds)
        
        arc-bounds
        (fn [[i j]]             ;; bounds to put on edge FROM i TO j
          (let [downstream
                (-> adjacency
                    (update i disj j) ;; delete edge
                    (update j disj i) ;; delete rev-edge in case
                    (reachable-from #{j}))
                
                upstream
                (-> inv-adjacency
                    (update i disj j)
                    (update j disj i)
                    (reachable-from #{i}))
                ]

            ;; so now we have vertices reachable from j (the far side of the edge)
            ;; and vertices from which i is reachable (the near side of the edge)
            (set-bounds upstream downstream)
            ))

        ]
    ;; consider using pmap here?
    (let [all-arcs (mapcat (juxt (juxt :i :j) (juxt :j :i)) edges)
          solution (into {} (pmap (fn [a] [a (arc-bounds a)]) all-arcs))
          ]
      (log/info "Bounds computed")
      solution
      )))

(comment
  (= (compute-bounds
      {:vertices
       [{:id 1 :supply {:capacity-kw 62}} ;; 62 = diversified 100, since we will un-diversify here
        {:id 2 :demand {:kwh 50 :kwp 75 :count 1}}
        {:id 3 :demand {:kwh 50 :kwp 75 :count 1}}]

       :edges ;; Y-shaped
       [{:i 1 :j :J}
        {:i :J :j 2}
        {:i :J :j 3}]})
     )
  
  )
