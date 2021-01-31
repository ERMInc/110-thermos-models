(ns thermos.opt.net.diversity)

(defn diversity-factor [problem]
  (let [limit (:diversity-limit problem 0.62)
        rate (:diversity-rate problem 1.0)]
    (fn [n]
      (/ (Math/round
          (* 100.0
             (+ limit
                (/ (- 1 limit)
                   (max 1 (* rate n))))))
         100.0))))
