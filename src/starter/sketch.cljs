(ns starter.sketch)

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
               online-analytics
               offline-analytics
               analyzer-buffer-size
               sound
               width
               height]}]
    (let [state (atom {:start-time nil
                       :vol-history '()})]
      {:setup (fn []
                (swap! state assoc
                       :start-time (.millis sketch))
                (.createCanvas sketch width height)
                (when sound
                  (.play sound)))
       :draw (fn []
               (let [local-time (- (.millis sketch)
                                   (:start-time @state))
                     index (long (/ local-time 1000 (/ analyzer-buffer-size 44100)))]
                 (when-let [rms (or (get online-analytics "rms")
                                    (get-in offline-analytics
                                            ["rms" index]))]
                   (swap! state update
                          :vol-history (fn [history]
                                         (let [new-history (conj history rms)]
                                           (if (> (count new-history)
                                                  (.-width sketch))
                                             (drop-last new-history)
                                             new-history))))
                   (loudness-viz sketch
                                 (reverse (:vol-history @state))))))})))