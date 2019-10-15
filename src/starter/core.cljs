(ns starter.core
  (:require [reagent.core :as r]
            [starter.p5 :as p5-helper]
            [starter.sketch :as p5-sketch]
            [starter.sketch-example :as sketch-example ]
            [starter.sound-processing :as sound-processing]))

(defn on-sound-loaded
  "TODO"
  [model sound]
  (js/console.log "LOADING COMPLETE")
  (swap! model assoc
         :sound sound)
  (sound-processing/offline-analyze-sound {:sound sound
                                           :on-analyzing (fn [progress]
                                                           (swap! model assoc
                                                                  :analyzing-process progress))
                                           :on-analyzed (fn [analytics-data]
                                                          (swap! model assoc
                                                                 :offline-analytics analytics-data))}))

(defn on-sound-loading
  [model process]
  (swap! model assoc
         :loading-process process))

(defn app []
  (let [model (r/atom {:offline-analytics []
                       :width 200
                       :height 200
                       :num-bands 5
                       :smoothing 1
                       :spacing 1})
        online-analytics (atom nil)]
    (r/create-class
      {:component-did-mount (fn []

                              #_(p5-helper/load-sound
                                  {:file "/radio-show.mp3"
                                   :on-loaded (partial on-sound-loaded model)
                                   :on-loading (partial on-sound-loading model)})

                              (let [audio-ctx (new js/AudioContext)
                                    audio-source (.createMediaElementSource audio-ctx (js/document.getElementById "audio"))]
                                (sound-processing/online-analyze-sound {:audio-ctx audio-ctx
                                                                        :audio-source audio-source
                                                                        :analytics-data online-analytics}))
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
                    :src "/example2.mp3"}]
                  [:select
                   {:onChange (fn [e]
                                (swap! model assoc :sketch e.target.value))}
                   [:option
                    {:value "v1"}
                    "Amplitude over time"]
                   [:option
                    {:value "v2"}
                    "All"]]
                  [:div (str "Loading... " (int (* (:loading-process @model)
                                                   100)))]
                  [:div (str "Analyzing... " (int (* (:analyzing-process @model)
                                                     100)))]

                  [:div
                   {:style {:display "flex"}}
                   [:div
                    {:style {:display "flex"
                             :flex-direction "column"}}
                    [:span "width"]
                    [:input {:type "range"
                             :min 100
                             :max 1000
                             :value (:width @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :width (int e.target.value)))}]
                    [:span "height"]
                    [:input {:type "range"
                             :min 100
                             :max 1000
                             :value (:height @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :height (int e.target.value)))}]
                    [:span "Number bars"]
                    [:input {:type "range"
                             :min 1
                             :max 100
                             :value (:num-bands @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :num-bands (int e.target.value)))}]
                    [:span "Smoothing"]
                    [:input {:type "range"
                             :min 1
                             :max 8
                             :value (:smoothing @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :smoothing (int e.target.value)))}]

                    [:span "Spacing"]
                    [:input {:type "range"
                             :min 0
                             :max 10
                             :value (:spacing @model)
                             :onChange (fn [e]
                                         (swap! model assoc
                                                :spacing (int e.target.value)))}]]
                   [:div {:style {:position "relative"
                                  :width "fit-content"
                                  :height "fit-content"}}
                    [:div {:style {:position "absolute"
                                   :width "100%"
                                   :height "100%"
                                   :z-index -1
                                   :background-image "url(https://images.unsplash.com/photo-1506704888326-3b8834edb40a)"
                                   :background-size "cover"}}]
                    [p5-helper/react-p5-sketch (merge {:sketch sketch-example/amplitude-over-time
                                                       :online-analytics online-analytics
                                                       ;;:offline-analytics (:offline-analytics @model)
                                                       }
                                                      @model)]]]])})))

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))


