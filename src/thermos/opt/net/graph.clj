(ns thermos.opt.net.graph
  "Graph functions for network model."
  )

(comment
  (defn- rand-graph [size degrees]
    (let [nodes (range size)]
      (->> (for [n nodes]
             [n (take (rand-nth degrees) (shuffle nodes))])
           (into {}))))

  (defn- grid [size]
    (let [nodes (for [i (range size) j (range size)] [i j])]
      (reduce
       (fn [a n]
         
         (assoc
          a n #{
                (update n 0 dec)
                (update n 0 inc)
                (update n 1 dec)
                (update n 1 inc)}
          )
         )
       {} nodes)
      )
    )

  (def ten-grid (grid 10))
  
  (def big-graph
    (rand-graph 2000 [1 1 1 1 1 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 5 6])
    )
  )

(defn bridges
  "Find the bridges in a graph. Within a problem the bounds are determined by:
  - If an edge is a bridge, what's on either side
  - If an edge is not a bridge, the members of its connected component

  Could an adjusted version of this function also compute the values
  as it goes along? Worth a think.
  "
  [graph]
  (let [t       (volatile! 0)   ;; counter for what vertex we are on in DFS
        visited (volatile! #{}) ;; where have we been
        tin     (volatile! {})  ;; what was 't' when we entered a vertex
        low     (volatile! {})  ;; min(tin[v], tin[ancestors of v]), i.e.
                                ;; the earliest tin of an ancestor

        ;; this is doing dfs on the stack, so it might cause some problems
        ;; on a graph with many edges.
        dfs     (fn dfs [v prev]
                  (vswap! visited conj v)
                  (let [time (vswap! t inc)]
                    (vswap! tin assoc v time)
                    (vswap! low assoc v time))

                  (reduce
                   (fn adj-loop [bridges to]
                     (cond
                       (= to prev) bridges

                       (contains? @visited to)
                       (do (vswap! low update v min (get @tin to))
                           bridges)
                       
                       :else
                       (into
                        (let [nxt (dfs to v)]
                          (vswap! low update v min (get @low to))

                          (cond-> nxt
                            ;; low[to] can only be more than tin[v] if
                            ;; there was no other way to get to to
                            ;; than from v, because we know we did DFS
                            ;; from v's ancestors already, so if you
                            ;; can get to to from one of them low[to]
                            ;; will tell us so.
                            (> (get @low to) (get @tin v))
                            (conj [v to])))
                        
                        bridges)))
                   nil
                   (graph v)))
        ]
    (reduce
     (fn vtx-loop [a v]
       (if (get @visited v)
         a
         (into (dfs v nil) a)))
     nil
     (keys graph))))

(comment
  (bridges
   {:a #{:b}
    :b #{:c}
    :c #{:d}
    :d #{:b}
    }
   )
  
  )

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

(comment
  ;; for example:
  (=
   (invert-adjacency-map
    {1 #{2 3} 4 #{2 6}})
   {2 #{1 4} 3 #{1} 6 #{4}}))

(defn close-adjacency-map
  "Compute the fixed point of adj under itself. Return a map like adj
  but where every vertex maps to all vertices reachable from it,
  rather than just its edges.

  This is subject to a rule that a single DFS traversal cannot contain
  both i->-j and j->-i for any i, j.

  It is required that there is no vertex in the input whose ID is a
  tuple of two other vertex IDs, because the algorithm uses such
  tuples to represent edges

  Invoked with `roots` it only computes values for things reachable
  from roots, which makes it a bit quicker.
  "
  ([adj] (close-adjacency-map adj (keys adj)))
  ([adj roots]
   (let [sconcat (comp set concat)
         sconj   (comp set conj)
         seen (volatile! #{})
         under (volatile! {})
         dfs! (fn dfs! [path-verts path-edges x]
                (cond

                  ;; First, if we have a cycle, we say that everything above x
                  ;; can reach everything x can reach, but then we stop
                  
                  (contains? path-verts x)
                  (doseq [p path-verts]
                    (vswap! under
                            update p
                            (fn [ps]
                              ;; from p, we can reach all x can reach, and x, but not p
                              ;; (although p is reachable from p, that is not something
                              ;; in the contract for the function).
                              (-> ps (sconcat (@under x) (list x)) (disj p)))))

                  ;; Next as a shortcut, if we already completed a traversal from x
                  ;; we can reuse its result. This is a diamond in the graph.
                  (@seen x)
                  (doseq [p path-verts]
                         (vswap! under update p sconcat (@under x) (list x)))

                  ;; Otherwise we need to do the traversal
                  :else
                  (do
                    ;; first, everywhere in the path-verts can reach x
                    (doseq [p path-verts] (vswap! under update p sconj x))
                    ;; next process all adjacent vertices, with x on the path-verts
                    (doseq [a (adj x)]
                      ;; we filter out any edge which we have already
                      ;; come down because in our flow problem we
                      ;; cannot use any edge in both directions.
                      (when-not (contains? path-edges [a x])
                        (dfs! (conj path-verts x)
                              (conj path-edges [x a]) a)))
                    ;; and now we have visited everywhere under x
                    (vswap! seen conj x))
                  
                  ))]
     (doseq [k roots] (dfs! #{} #{} k))
     @under)))

(defn make-undirected [adj]
  (persistent!
   (reduce
    (fn [out [i js]]
      (reduce
       (fn [out j] (assoc! out j (sconj (get out j) i)))
       out js))
    (transient adj) adj)))

(defn label-components
  "Return a map from vertex ID to an identity for the component it is in"
  [adj]
  (let [adj (make-undirected adj)
        components (volatile! {})
        next-id (volatile! 0)
        mark! (fn mark! [x id]
               (when-not (contains? @components x)
                 (let [id (or id (vswap! next-id inc))]
                   (vswap! components assoc x id)
                   (doseq [n (adj x)] (mark! n id)))))
        
        ]
    (doseq [k (keys adj)] (mark! k nil))
    @components))
