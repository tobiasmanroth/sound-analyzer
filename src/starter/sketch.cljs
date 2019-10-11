(ns starter.sketch
  (:require ["p5" :as p5]
            ["p5/lib/addons/p5.sound" :as p5-sound]
            [starter.sound-processing :as sound-processing]))

(defn loudness-viz
  [sketch vol-history]
  (let [height (.-height sketch)]
    (doto sketch
      (.clear)
      (.stroke 255)
      (.noFill)
      (.push)
      (.beginShape))
    (doall (map-indexed
             (fn [i vol]
               (let [y (.map sketch vol 0 1 (/ height 2) 0)]
                 (.vertex sketch i, y)))
             vol-history))
    (.endShape sketch)))

(def my-sketch
  (fn [{:keys [sketch
               offline-analytics
               analyzer-buffer-size
               sound]}]
    (let [state (atom {:start-time nil
                       :vol-history '()})]
      {:setup (fn []
                (swap! state assoc
                       :start-time (.millis sketch))
                (.createCanvas sketch 200 200)
                (.play sound))
       :draw (fn []
               (let [local-time (- (.millis sketch)
                                   (:start-time @state))
                     index (long (/ local-time 1000 (/ analyzer-buffer-size 44100)))]
                 (when-let [rms (get-in offline-analytics
                                        [index "rms"])]
                   (swap! state update
                          :vol-history (fn [history]
                                         (let [new-history (conj history rms)]
                                           (if (> (count new-history)
                                                  (.-width sketch))
                                             (drop-last new-history)
                                             new-history))))
                   (loudness-viz sketch
                                 (reverse (:vol-history @state))))))})))