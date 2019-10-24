(ns starter.math-util)

(defn median
  "https://rosettacode.org/wiki/Averages/Median"
  [numbers]
  (let [numbers (sort numbers)
        cnt (count numbers)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth numbers mid)
      (/ (+ (nth numbers mid)
            (nth numbers
                 (dec mid))) 2))))

(defn average
  [numbers]
  (/ (apply + numbers)
     (count numbers)))

(defn clamp
  "Clamps `value` between minimum and maximum."
  [value minimum maximum]
  (min (max value
            minimum)
       maximum))
