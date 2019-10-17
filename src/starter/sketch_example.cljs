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


(defonce state (atom {:local-time 0
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
  [time dest-ctx]
  (js/Promise. (fn [resolve reject]
                 (swap! state assoc
                        :local-time time
                        :rendering true)
                 (.redraw (:p5-sketch @state))
                 (.drawImage dest-ctx
                             (js/document.querySelector "#defaultCanvas0")
                             0 0)
                 (resolve))))

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

(defn my-sketch
  "TODO"
  [params]
  (let [params* (merge default-params
                       params)
        vol-history-count (* (max (:smoothing params*) 1)
                             (:num-bands params*))]
    {:setup (fn [sketch]
              (let [p5-canvas (.createCanvas sketch
                                             (:width params*)
                                             (:height params*))]
                (swap! state assoc
                       :p5-canvas (:canvas p5-canvas)
                       :p5-sketch sketch
                       :start-time (.millis sketch)))
              (doto sketch
                (.rectMode (.-CENTER sketch))
                (.colorMode (.-HSB sketch))
                (.noLoop)))
     :draw (fn []
             (let [sketch (:p5-sketch @state)]
               (.background sketch (rand-int 255) 255 255)

               #_(doseq [r (range 1000)]
                 (.ellipse sketch
                           (rand-int width)
                           (rand-int height)
                           20
                           20))
               (let [index (analyzer-data-index (:local-time @state)
                                                (:analyzer-buffer-size params*)
                                                (:sample-rate params*))
                     offline-analytics (get-in @state
                                               [:offline-analytics "rms"])
                     data-points (->> offline-analytics
                                      (take index)
                                      (drop (- index
                                               vol-history-count))
                                      (reverse))]
                 ;;(js/console.log (pr-str data-points))
                 (loudness-viz sketch
                               data-points
                               params*))

               (.rect sketch 0 0 100 100)
               (.text sketch (:local-time @state) 10 25))

             (comment
               (let [sketch (:p5-sketch @state)]

                 #_(case (:render-mode @state)
                     :offline
                     :online (do
                               (swap! state update
                                      :local-time (- (.millis sketch)
                                                     (:start-time @state))
                                      :vol-history (fn [history]
                                                     (let [new-history (conj history rms)]
                                                       (if (> (count new-history)
                                                              vol-history-count)
                                                         (drop-last new-history)
                                                         new-history)))))
                     ;; save volume history

                     )

                 #_(when-let [rms (get-in state
                                          [:offline-analytics "rms" index])]

                     )
                 ))
             )}))






#_(def default-params
  {:smoothing 2
   :num-bands 5
   :spacing 1
   :width 200
   :height 200
   :analyzer-buffer-size 512
   :sample-rate 44100})

#_(defn amplitude-over-time [{:keys [offline-analytics local-time] :as params}]
  (let [params* (merge default-params
                       params)
        vol-history-count (* (max (:smoothing params*) 1)
                             (:num-bands params*))
        state (atom {:vol-history (repeat vol-history-count 0)})
        sketch (atom nil)]

    {:set-time! (fn [local-time]
                  (swap! state assoc
                         :local-time local-time)
                  ;;(.redraw @sketch)
                  )

     :setup (fn [sketch*]
              (reset! sketch sketch*)
              (swap! state assoc
                     :start-time (.millis @sketch))
              (doto @sketch
                (.createCanvas (:width params*)
                               (:height params*))
                (.rectMode (.-CENTER @sketch))
                (.colorMode (.-HSB @sketch))

                (.noLoop)
                ))
     :draw (fn []
             (let [index (long (/ (:local-time params*)
                                  1000
                                  (/ (:analyzer-buffer-size params*)
                                     (:sample-rate params*))))]

               (js/console.log (:local-time params*))

               (when-let [rms (get-in offline-analytics
                                      ["rms" index])]
                 (swap! state update
                        :vol-history (fn [history]
                                       (let [new-history (conj history rms)]
                                         (if (> (count new-history)
                                                vol-history-count)
                                           (drop-last new-history)
                                           new-history))))

                 (loudness-viz @sketch
                               (:vol-history @state)
                               params*))))}))

#_(def amplitude-over-time
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
