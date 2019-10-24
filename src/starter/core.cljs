(ns starter.core
  (:require [reagent.core :as r]
            ["p5" :as p5]
            [starter.p5 :as p5-helper]
            [starter.sketch-example :as sketch-example ]
            [starter.sound-processing :as sound-processing]))

(defn animation-loop
  [render-function]
  (js/requestAnimationFrame
    (fn animation-loop-callback [_real-time]
      (render-function)
      (js/requestAnimationFrame animation-loop-callback))))

(declare resolve-init-promise)

(def init-promise
  ;; Mechanism to allow the first call of
  ;; `storrito.recorder.route/render-frame` to wait until the `init!`
  ;; process is done.
  (js/Promise.
    (fn [resolve reject]
      (def resolve-init-promise
        resolve))))

(defn render-time!
  [time]
  (js/Promise.all
    [(sketch-example/render-frame! time)]))

(defn cut-sound
  ""
  [sound-buffer start-time-in-ms end-time-in-ms]
  (let [channel-count (.-numberOfChannels sound-buffer)
        sample-rate (.-sampleRate sound-buffer)
        start-offset (* (/ start-time-in-ms
                           1000) sample-rate)
        end-sample (* (/ end-time-in-ms
                         1000) sample-rate)
        buffer-length (- end-sample start-offset)
        audio-ctx (new js/AudioContext)
        cut-buffer (.createBuffer audio-ctx
                              channel-count
                              buffer-length
                              sample-rate)
        transition-array (new js/Float32Array buffer-length)]

    (doseq [channel (range channel-count)]
      (.copyFromChannel sound-buffer transition-array
                        channel
                        start-offset)
      (.copyToChannel cut-buffer transition-array
                      channel
                      0))
    cut-buffer))

(defn init!
  "Returns a `js/Promise` that analyzes the sound"
  [sketch-params]
  (-> (p5-helper/load-sound
        {:file "/radio-show.mp3"})
      (.then (fn [sound]
               (let [sound-buffer-snippet (cut-sound (.-buffer sound)
                                                     1000 2000)]
                 (sound-processing/offline-analyze-sound sound-buffer-snippet))))
      (.then (fn [offline-analytics]
               (swap! sketch-example/state assoc
                      :offline-analytics offline-analytics)
               (p5. (p5-helper/p5-sketch
                      (sketch-example/my-sketch sketch-params))
                    "p5-stuff")
               ))))

(defn reload-p5-sketch [params]
  (sketch-example/remove!)
  (p5. (p5-helper/p5-sketch
         (sketch-example/my-sketch params))
       "p5-stuff"))

(defn ^:export render-frame
  [frame-rate frame-number]
  ;; the `init-promise` is resolved as soon as the initial loading of
  ;; the fabric-canvas is done:
  (-> init-promise
      (.then (fn []
               ;; Render frame of time in ms
               (render-time! (* frame-number
                                (/ 1 frame-rate)
                                1000))))))

(defn p5-canvas
  [{:keys [model id]}]
  (r/create-class
    {:component-did-mount
     (fn []
       ;; For offline rendering:
       (.then (init! {:editor-canvas id})
                (fn []
                  (resolve-init-promise)))

       ;; For live preview:
       #_(p5. (p5-helper/p5-sketch
              (sketch-example/my-sketch {:editor-canvas id}))
            "p5-stuff")
       )
     :render (fn []
               [:canvas
                {:id id
                 :width (:width @model)
                 :height (:height @model)}])}))

(defn editor-panel
  []
  (let [canvas-id "p5-canvas"
        model (r/atom {:width 200
                       :height 200
                       :num-bands 5
                       :smoothing 1
                       :spacing 1
                       :frame 0})]
    (r/create-class
      {:component-did-mount (fn []
                              #_(let [audio-ctx (new js/AudioContext)
                                      audio-source (.createMediaElementSource audio-ctx (js/document.getElementById "audio"))]
                                  (sound-processing/online-analyze-sound {:audio-ctx audio-ctx
                                                                          :audio-source audio-source
                                                                          :analyzer-callback sketch-example/push-live-rms-value!})
                                  (animation-loop (fn []
                                                    (sketch-example/render-next-frame!)))
                                  )
                              )
       :render (fn []
                 [:div
                  [:h1 "Audio Visualizer 0.11"]
                  [:audio
                   {:controls true
                    "autoPlay" false
                    :loop true
                    "crossOrigin" "anonymous"
                    :id "audio"
                    :src "/radio-show.mp3"}]

                  [:div
                   {:style {:display "flex"}}
                   [:div
                    {:style {:display "flex"
                             :flex-direction "column"}}
                    [:span "Frame"]
                    [:input {:type "range"
                             :min 0
                             :max 60
                             :value (:frame @model)
                             :onChange (fn [e]
                                         (let [frame (int e.target.value)]
                                           (swap! model assoc
                                                  :frame frame)
                                           (render-frame 60 frame)))}]
                    [:span "width"]
                    [:input {:type "range"
                             :min 100
                             :max 1000
                             :value (:width @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :width (int e.target.value))
                                         (reload-p5-sketch @model))}]
                    [:span "height"]
                    [:input {:type "range"
                             :min 100
                             :max 1000
                             :value (:height @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :height (int e.target.value))
                                         (reload-p5-sketch @model))}]
                    [:span "Number bars"]
                    [:input {:type "range"
                             :min 1
                             :max 100
                             :value (:num-bands @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :num-bands (int e.target.value))
                                         (reload-p5-sketch @model))}]
                    [:span "Smoothing"]
                    [:input {:type "range"
                             :min 1
                             :max 8
                             :value (:smoothing @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :smoothing (int e.target.value))
                                         (reload-p5-sketch @model))}]
                    [:span "Spacing"]
                    [:input {:type "range"
                             :min 0
                             :max 10
                             :value (:spacing @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :spacing (int e.target.value))
                                         (reload-p5-sketch @model))}]]
                   [:div {:style {:position "relative"
                                  :width "fit-content"
                                  :height "fit-content"}}
                    [:div {:style {:position "absolute"
                                   :width "100%"
                                   :height "100%"
                                   :z-index -1
                                   :background-image "url(https://images.unsplash.com/photo-1506704888326-3b8834edb40a)"
                                   :background-size "cover"}}]
                    [p5-canvas {:id "p5-editor-canvas"
                                :model model}]
                    ]]])})))

(defonce state (atom {}))

(defn app []
  (let [hide-p5-canvas (r/atom true)]
    (fn []
      [:<>
       [editor-panel]
       [:input {:checked @hide-p5-canvas
                :type "checkbox"
                :onChange (fn [e]
                            (let [hide? e.target.checked]
                              (reset! hide-p5-canvas hide?)))}]
       [:span "Hide p5 canvas"]
       [:div
        {:id "p5-stuff"
         :style (when @hide-p5-canvas {:display "none"})}]])))

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))
