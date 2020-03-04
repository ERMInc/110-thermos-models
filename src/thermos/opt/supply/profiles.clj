(ns thermos.opt.supply.profiles
  "Use scale-to-fit to adjust a profile shape so it has a given AUC and peak"
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :refer [>defn >defn- => |]]))

(s/def ::values-profile
  (s/coll-of
   (s/keys :req-un [::frequency ::values])))
(s/def ::frequency pos-int?)
(s/def ::values (s/coll-of double? :kind vector?))

(>defn profile-peak ^double [profile]
  [::values-profile => double?]
  (reduce max (mapcat :values profile)))

(>defn profile-area ^double [profile]
  [::values-profile => double?]
  (reduce + (for [day profile]
              (let [slice-size (/ 1.0 (count (:values day)))]
                (* slice-size
                   (double (:frequency day 1.0))
                   (reduce + 0.0 (:values day)))))))

(let [delta 0.1]
  (>defn- near= [^double a ^double b]
    [double? double? => boolean?]
    (<= (Math/abs (- a b)) delta)))

(>defn compress-profile [profile ^double α]
  [::values-profile double? => ::values-profile]
  (for [day profile]
    (update day :values
            (fn [vs] (mapv #(Math/pow % α) vs)))))

(>defn scale-profile [profile ^double scale]
  [::values-profile double? => ::values-profile]
  (for [day profile]
    (update day :values (fn [vs] (mapv #(double (* % scale)) vs)))))

(>defn norm-profile [profile]
  [::values-profile => ::values-profile]
  (scale-profile profile (/ 1.0 (profile-peak profile))))

(>defn scale-to-fit
  "`profile` should be a list of maps having :frequency and :values.
  Each map is a representative type of a day, which occurs :frequency
  often. :values are equal-spaced intervals within the day.

  Returns a same-shaped structure in which
  a) the maximum value in any interval is `target-peak`
  b) the area-under-curve if we multiply by :frequencies is `target-area`
     This AUC incorporates differing interval-widths within different day types,
     accomodating different 'precision' on different days.

     The units for the target area are the same as for frequency (i.e. days)
     so if you're doing kWh you need to turn it into kWd.
  "
  [profile target-area target-peak]
  [::values-profile double? double?
   => ::values-profile | #(and (near= (profile-peak %) target-peak)
                               (near= (profile-area %) target-area))
   ]
  ;; Marko's hack is to say norm(curve)^α; solve for α s.t. area is whatever it needs to be
  (let [normalized   (norm-profile profile)
        normed-target-auc (/ target-area target-peak)]
    (loop [lower 0.0    ;; this lower bound makes the curve = 1
                        ;; everywhere, can't get flatter than that
           upper 1000.0 ;; this upper bound will produce only the
                        ;; peak, since even 0.95^1000 is ~zero
           n 0          ;; termination counter, just in case
           ]
      (let [α (* 0.5 (+ lower upper))
            new-profile (compress-profile normalized α)
            new-auc     (profile-area new-profile)]
        (cond
          (or
           (> n 15)
           (= lower upper)
           (near= new-auc normed-target-auc))
          
          (scale-profile new-profile target-peak)

          (< new-auc normed-target-auc)
          ;; our α value is too big; we need to squash the shape less
          (recur lower α (inc n))

          (> new-auc normed-target-auc)
          ;; our α value is too small; we need to squash the shape more
          (recur upper α (inc n))
          )))))

(defn combine-buildings
  [day-types ;; {day type => frequency}
   profiles  ;; {profile => {day type => values}}
   demands   ;; [{:profile p :kwh a :kwp p}]
   target-kwh target-peak]
  (let [;; this assumes profiles are heterogenous
        day-type-length (->> (for [[t v] (second (first profiles))]
                               [t (count v)])
                             (into {}))
        
        day-type-order (sort (keys day-types))
        zero           (for [d day-type-order]
                         (vec (repeat (day-type-length d) 0)))

        combined-profile ;; [[double]]
        (reduce
         (fn [acc {pr :profile kwh :kwh kwp :kwp}]
           (let [profile (get profiles pr)
                 profile (-> (for [d day-type-order]
                               {:frequency (get day-types d 1.0)
                                :values    (get profile d)})
                             (scale-to-fit (/ kwh 24.0) kwp))]
             (map
              (fn [da dp] (mapv + da (:values dp)))
              acc profile)))
         zero
         demands)

        combined-profile
        (-> (map (fn [d vs]
                   {:day-type d
                    :frequency (get day-types d 1.0)
                    :values vs})
                 day-type-order combined-profile)
            (scale-to-fit (/ target-kwh 24.0) target-peak)
            (->> (reduce #(assoc %1 (:day-type %2) (dissoc %2 :day-type)) {})))
        ]
    combined-profile))


(defn create-input-profile
  "Our profile is stored in a different format for presentation, so we
  need to do the mangling to make it have the right peak, and then we
  need to reassemble it into a single input profile of the sort that
  goes in a supply problem.
  "
  [profile buildings target-demand target-peak]
  (let [day-types     (:day-types profile)
        heat-profiles (:heat-profiles profile)

        system-profile (combine-buildings
                        (:day-types profile)
                        (:heat-profiles profile)
                        buildings ;; somewhere we need to transform
                        ;; from namespaced keywords
                        target-demand target-peak)]
    (->> (for [[day frequency] day-types]
           [day
            {:frequency frequency
             :heat-demand (:values (get system-profile day))
             :grid-offer  (get (:grid-offer profile) day)
             :fuel
             (->> (for [[fuel prices] (:fuel-prices profile)]
                    [fuel (merge {:price (get prices day)}
                                 (-> profile :emissions-factors (get fuel) (get day)))])
                  (into {}))}])
         (into {}))))

