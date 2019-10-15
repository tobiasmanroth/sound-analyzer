(ns starter.sketch-example
  (:require [starter.p5 :as p5-helper]
            [starter.math-util :as math-util]))

(defn loudness-viz
  [sketch vol-history {:keys [num-bands spacing] :as params}]
  (let [height (.-height sketch)
        width (.-width sketch)
        min-height 2
        w (/ width (* (+ num-bands
                         spacing)
                      2))
        roundness (/ w 2)
        gap (* w (/ spacing num-bands))
        volumes (map
                  (fn [numbers]
                    ;;(apply max numbers)
                    ;;(math-util/median numbers)
                    (math-util/average numbers))
                  (partition-all (int (/ (count vol-history)
                                         num-bands))
                                 vol-history))]
    (doto sketch
      (.clear)
      (.noFill)
      (.stroke 0 0))

    (doall
      (map-indexed
        (fn [i volume]
          (let [x (+ (/ width 2.0)
                     (* i gap)
                     (* i w)
                     (/ (+ w gap) 2))
                h (.map sketch volume
                        0
                        1
                        min-height
                        height)
                d (max 0 (.map sketch (max (/ num-bands 2)
                                           (+ i 1))
                               0
                               num-bands
                               1
                               0.1))
                alpha (p5-helper/logMap i
                                        0
                                        (- num-bands 1)
                                        1
                                        255)
                hue (.map sketch i
                          0
                          num-bands
                          236
                          100)
                saturation (.map sketch i
                                 0
                                 num-bands
                                 34
                                 47)
                brightness (.map sketch i
                                 0
                                 num-bands
                                 70
                                 97)]
            (doto sketch
              (.fill hue saturation brightness alpha)
              (.rect x (/ height 2) w (* h d 5) roundness)
              (.rect (- width x) (/ height 2) w (* h d 5) roundness)
              )))
        volumes))))

(def default-params
  {:smoothing 2
   :num-bands 5
   :spacing 1
   :width 200
   :height 200})

(def amplitude-over-time
  (fn [{:keys [sketch
               online-analytics
               offline-analytics
               analyzer-buffer-size
               sound] :as params}]
    (let [params* (merge default-params
                         params)
          vol-history-count (* (max (:smoothing params*) 1)
                               (:num-bands params*))
          state (atom {:start-time nil
                       :vol-history (repeat vol-history-count 0)})]
      {:setup (fn []
                (swap! state assoc
                       :start-time (.millis sketch))
                (doto sketch
                  (.createCanvas (:width params*)
                                 (:height params*))
                  (.rectMode (.-CENTER sketch))
                  (.colorMode (.-HSB sketch))))
       :draw (fn []
               (let [local-time (- (.millis sketch)
                                   (:start-time @state))
                     index (long (/ local-time 1000 (/ analyzer-buffer-size 44100)))]
                 (when-let [rms (or (get @online-analytics "rms")
                                    (get-in offline-analytics
                                            ["rms" index]))]
                   (swap! state update
                          :vol-history (fn [history]
                                         (let [new-history (conj history rms)]
                                           (if (> (count new-history)
                                                  vol-history-count)
                                             (drop-last new-history)
                                             new-history))))

                   (loudness-viz sketch
                                 (:vol-history @state)
                                 params*))))})))
