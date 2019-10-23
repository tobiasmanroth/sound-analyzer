(ns starter.core
  (:require [reagent.core :as r]
            ["p5" :as p5]
            [starter.p5 :as p5-helper]
            [starter.sketch-example :as sketch-example ]
            [starter.sound-processing :as sound-processing]))

(defn on-sound-loaded
  "TODO"
  [sound]
  (js/Promise
    (fn [resolve reject]
      (sound-processing/offline-analyze-sound {:sound sound
                                               :on-analyzed (fn [analytics-data]
                                                              (resolve analytics-data))}))))

#_(defn play-loop
  "Starts a `js/requestAnimationFrame` loop to play a timeline. The
   browser renders a frame approximately every 16.66 milliseconds (60
   FPS). Adds the current `(js/performance.now)` as `:time/now` to the
   map in the `state` Clojure atom, before it applies the function `f`
   to the `state` value. The loop is stopped, as soon as the flag
   `:time/stopped` is found in the state value."
  [state f]
  (js/requestAnimationFrame
    (fn raf []
      (if (:time/stopped @state)
        ;; provides middleware with side-effects the chance to stop
        ;; the playback of a video for example:
        (swap! state f)
        (do
          (swap! state (fn [state-value]
                         (f (assoc state-value
                              :time/now
                              (js/performance.now)))))
          (js/requestAnimationFrame raf))))))

#_(play-loop local-time
             (fn [time]
               (sketch-example/render-frame! (:time/now time)
                                             (.getContext
                                               (js/document.querySelector "#canvas")
                                               "2d"))
               time))

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
    (doall
      (map
        (fn [canvas]
          (sketch-example/render-frame! time
                                        canvas))
        [(js/document.querySelector "#canvas")]))))

(comment
  (def local-time
    (atom 0))
  (frame-seek! (swap! local-time
                      + 0.1))
  )

(defn init!
  "Returns a `js/Promise` that analyzes the sound"
  []
  (-> (p5-helper/load-sound
        {:file "/radio-show.mp3"})
      (.then (fn [sound]
               (sound-processing/offline-analyze-sound {:sound sound})))
      (.then (fn [offline-analytics]
               (swap! sketch-example/state assoc
                      :offline-analytics offline-analytics)
               (p5. (p5-helper/p5-sketch
                      (sketch-example/my-sketch {}))
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

(defn p5-canvas [model]
  (r/create-class
    {:component-did-mount
     (fn []
       (.then (init!)
              (fn []
                (resolve-init-promise)))
       (js/console.log "CANVAS MOUNTED")
       )
     :component-did-update
     (fn []
       (js/console.log "UPDATE"))

     :render (fn []
               (js/console.log "RENDER")
               [:canvas#canvas {:width (:width @model)
                                :height (:height @model)}])}))

(defn editor-panel
  []
  (let [model (r/atom {:offline-analytics nil
                       :width 200
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
                                                                          :analytics-data online-analytics}))
                              )
       :render (fn []
                 [:div
                  [:h1 "Audio Visualizer 0.11"]
                  #_[:audio
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
                             :max 10000
                             :value (:frame @model)
                             :onChange (fn [e]
                                         (let [frame (int e.target.value)]
                                           (swap! model assoc
                                                  :frame frame)
                                           (render-frame 30 frame)))}]
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
                    [p5-canvas model]
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


