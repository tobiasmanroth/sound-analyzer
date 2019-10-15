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

(defn sharpness-color
  [sharpness]
  [(+ (* (- 1 sharpness) 128)
      128)
   (+ (* (- 1 sharpness) 128)
      128)
   (+ (* sharpness 128)
      128)])

(def maxi (atom 0))

(defn spectrum-viz
  [sketch spectrum]
  (let [width (.-width sketch)
        height (.-height sketch)]
    (.noStroke sketch)
    (.fill sketch 0 255 0)
    (doall
      (map-indexed
        (fn [i f]
          (let [x (* (js/Math.sqrt (/ (+ i 1) (count spectrum)))
                     width)
                y height
                w (* (js/Math.sqrt (/ i (count spectrum)))
                     width)
                h (* -1 (/ f 128) height)]
            (when (< @maxi f)
              (reset! maxi f))
            (.rect sketch x y w h)))
        spectrum))))

(defn wave-viz
  [sketch waveform]
  (let [width (.-width sketch)
        height (.-height sketch)]
    (doto sketch
      (.noFill)
      (.beginShape)
      (.stroke 255 255 255)
      (.strokeWeight 4))
    (doall
      (map-indexed
        (fn [i f]
          (let [x (* (/ i (count waveform))
                     width)
                y (+ (/ height 2)
                     (* f height))]
            (.vertex sketch x y)))
        waveform))
    (.endShape sketch)))

(defn sharpness-vis [sketch sharpness]
  (let [[r g b] (sharpness-color sharpness)]
    (.background sketch r g b)))

#_(defn audio-visualizer
    "Instance mode of p5: https://github.com/processing/p5.js/wiki/Global-and-instance-mode"
    [^js sketch]
    (set! (.-setup sketch)
          (fn []
            (.createCanvas sketch 800 600)))
    (set! (.-draw sketch)
          (fn []
            (let [waveform (get @analytics "buffer")
                  spectrum (get @analytics "powerSpectrum")
                  rms (get @analytics "rms")
                  sharpness (get @analytics "perceptualSharpness")]
              (sharpness-vis sketch sharpness)
              (spectrum-viz sketch spectrum)
              (loudness-viz sketch rms)
              (wave-viz sketch waveform)))))

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
                 (when-let [rms (or (get @online-analytics "rms")
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