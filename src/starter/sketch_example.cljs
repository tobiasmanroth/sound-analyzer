(ns starter.sketch-example
  (:require [starter.p5 :as p5-helper]
            [starter.math-util :as math-util]))

(defn loudness-viz
  "The actual sound visualization"
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
                        0 1
                        min-height height)
                d (max 0 (.map sketch (max (/ num-bands 2)
                                           (+ i 1))
                               0 num-bands
                               1 0.1))
                alpha (p5-helper/logMap i
                                        0 (- num-bands 1)
                                        1 255)
                hue (.map sketch i
                          0 num-bands
                          236 100)
                saturation (.map sketch i
                                 0 num-bands
                                 34 47)
                brightness (.map sketch i
                                 0 num-bands
                                 70 97)]
            (doto sketch
              (.fill hue saturation brightness alpha)
              (.rect x (/ height 2) w (* h d 5) roundness)
              (.rect (- width x) (/ height 2) w (* h d 5) roundness))))
        volumes))))


(defonce state
         (atom {:local-time 0
                :live-analytics nil
                :offline-analytics nil
                :start-time 0
                :p5-sketch nil}))

(def default-params
  {:smoothing 2
   :num-bands 5
   :spacing 1
   :width 200
   :height 200
   :analyzer-buffer-size 512
   :sample-rate 44100})

(defn render-frame!
  [time canvas]
  (js/Promise. (fn [resolve reject]
                 (swap! state assoc
                        :local-time time)
                 (.redraw (:p5-sketch @state))
                 (let [ctx (.getContext canvas "2d")
                       canvas-width (.-width canvas)
                       canvas-height (.-height canvas)]
                   (.clearRect ctx 0 0
                               canvas-width
                               canvas-height)
                   (.drawImage ctx
                               (js/document.querySelector "#defaultCanvas0")
                               0 0
                               canvas-width
                               canvas-height)
                   (resolve)))))

(defn render-next-frame!
  "Should be called in an animation loop for live preview."
  [canvas]
  (when (:p5-sketch @state)
    (.redraw (:p5-sketch @state))
    (let [ctx (.getContext canvas "2d")
          canvas-width (.-width canvas)
          canvas-height (.-height canvas)]
      (.clearRect ctx 0 0
                  canvas-width
                  canvas-height)
      (.drawImage ctx
                  (js/document.querySelector "#defaultCanvas0")
                  0 0
                  canvas-width
                  canvas-height))))

(defn remove!
  []
  (when (:p5-sketch @state)
    (.remove (:p5-sketch @state))))

(defn analyzer-data-index
  [local-time analyzer-buffer-size sample-rate]
  (long (/ local-time
           1000
           (/ analyzer-buffer-size
              sample-rate))))

(defn pad
  "Pads the `data-points` sequence with zeros to create a sequence with size of `seq-size`."
  [data-points seq-size]
  (if (< (count data-points)
         seq-size)
    (let [padding (repeat (- seq-size
                             (count data-points))
                          0)]
      (concat data-points padding))
    data-points))

(defn get-offline-analytics-data-points
  [{:keys [analyzer-buffer-size sample-rate]} vol-history-count]
  (let [offline-analytics (get-in @state
                                  [:offline-analytics "rms"])
        end-index (min (analyzer-data-index (:local-time @state)
                                            analyzer-buffer-size
                                            sample-rate)
                       (- (count offline-analytics) 1))
        start-index (max (- end-index
                            vol-history-count)
                         0)]
    (-> (subvec offline-analytics
                start-index
                end-index)
        (reverse))))

(defn push-live-rms-value!
  "TODO"
  [new-analytics]
  (let [new-rms (get new-analytics "rms")]
    (swap! state update-in [:live-analytics "rms"]
           (fn [old-rms-array]
             (let [new-rms-array (conj old-rms-array
                                       new-rms)]
               (if (> (count new-rms-array)
                      (:analytics-history-size @state))
                 (drop-last (- (count new-rms-array)
                               (:analytics-history-size @state))
                            new-rms-array)
                 new-rms-array))))))

(defn timestamp-viz [sketch]
  (doto sketch
    (.fill "white")
    (.rect 0 0 100 100)
    (.fill "black")
    (.text (/ (:local-time @state)
              1000)
           10 25)))

(defn my-sketch
  "TODO"
  [params]
  (let [params* (merge default-params
                       params)]
    {:setup (fn [sketch]
              (let [p5-canvas (.createCanvas sketch
                                             (:width params*)
                                             (:height params*))]
                (swap! state assoc
                       :analytics-history-size (* (max (:smoothing params*) 1)
                                                  (:num-bands params*))
                       :p5-canvas (:canvas p5-canvas)
                       :p5-sketch sketch
                       :start-time (.millis sketch)))
              (doto sketch
                (.rectMode (.-CENTER sketch))
                (.colorMode (.-HSB sketch))
                (.noLoop)))
     :draw (fn []
             (let [sketch (:p5-sketch @state)
                   data-points (pad (or (get (:live-analytics @state)
                                             "rms")
                                        (get-offline-analytics-data-points params*
                                                                           (:analytics-history-size @state)))
                                    (:analytics-history-size @state))]
               (loudness-viz sketch
                             data-points
                             params*)
               ;; (timestamp-viz sketch)
               ))}))
