(ns thermos.opt.supply.profiles-test
  (:require [thermos.opt.supply.profiles :as profiles]
            [thermos.opt.supply.core :as model]
            [clojure.test :as t]
            [com.rpl.specter :as S]))

(t/deftest scale-profile-noop
  (let [unscaled-profile [{:frequency 100 :divisions 24 :values (vec (repeat 24 1))}
                          {:frequency 100 :divisions 24 :values (vec (repeat 24 2))}]

        ;; so the unscaled peak is 2
        ;; the unscaled AUC for scale-to-fit is (in kWd): 
        ;; so scaling to that should be noop

        scaled-profile (profiles/scale-to-fit unscaled-profile
                                              (profiles/profile-area unscaled-profile)
                                              (profiles/profile-peak unscaled-profile))
        ]
    
    (t/is (profiles/near= (profiles/profile-area scaled-profile) 300))
    (t/is (profiles/near= (profiles/profile-peak scaled-profile) 2))))

(t/deftest scale-profile-value
  (let [unscaled-profile [{:frequency 100 :divisions 24 :values (vec (repeat 24 1))}
                          {:frequency 100 :divisions 24 :values (vec (repeat 24 2))}]]
    (dotimes [i 5]
      (let [target-area (* 100 (+ 3 i))
            target-peak (+ 2 i) ;; have to adjust this; some
                                ;; combinations of peak & area are
                                ;; insoluble without a method that
                                ;; changes the shape more cleverly,
                                ;; especially with really simple
                                ;; profile like written above.
            scaled-profile
            (profiles/scale-to-fit unscaled-profile
                                   target-area
                                   target-peak)

            observed-area (profiles/profile-area scaled-profile)
            observed-peak (profiles/profile-peak scaled-profile)
            ]
        (println target-area observed-area "  " target-peak observed-peak)
        (t/is (< (Math/abs (- observed-area target-area)) 2))
        (t/is (< (Math/abs (- observed-peak target-peak)) 0.1))))))
