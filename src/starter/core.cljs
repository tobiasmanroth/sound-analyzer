(ns starter.core
  (:require [reagent.core :as r]
            [starter.p5 :as p5-helper]
            [starter.sketch :as p5-sketch]
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
  (let [model (r/atom {:sketch "v3"
                       :offline-analytics []
                       :width 200
                       :height 200})
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
                    :src "/radio-show.mp3"}]
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
                  [:input {:type "range"
                           :min 100
                           :max 500
                           :value (:width @model)
                           :onChange (fn [e]
                                       (swap! model assoc
                                              :width e.target.value))}]
                  [:div {:style {:position "relative"
                                 :width "fit-content"
                                 :height "fit-content"}}
                   [:div {:style {:position "absolute"
                                  :width "100%"
                                  :height "100%"
                                  :z-index -1
                                  :background-image "url(https://images.unsplash.com/photo-1506704888326-3b8834edb40a)"
                                  :background-size "cover"}}]
                   [p5-helper/react-p5-sketch {:sketch p5-sketch/my-sketch
                                               :width (:width @model)
                                               :height (:height @model)
                                               :sound (:sound @model)
                                               :online-analytics online-analytics
                                               ;;:offline-analytics analytics
                                               }]]])})))

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))


