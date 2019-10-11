(ns starter.p5
  (:require [reagent.core :as r]
            ["p5" :as p5]
            ["meyda" :as m]
            ["p5/lib/addons/p5.sound" :as p5-sound]
            ["react-p5-wrapper" :as react-p5-wrapper]))

(def react-p5
  (r/adapt-react-class react-p5-wrapper/default))

(defn p5-sketch
  "TODO"
  [sketch params]
  (fn [^js p5-sketch-object]
    (let [{:keys [preload setup draw]} (sketch (merge params
                                                      {:sketch p5-sketch-object}))]
      (set! (.-preload p5-sketch-object)
            preload)
      (set! (.-setup p5-sketch-object)
            setup)
      (set! (.-draw p5-sketch-object)
            draw))))

(defn load-sound
  [{:keys [file on-loaded on-error on-loading]}]
  (let [sound (new (.-SoundFile p5)
                   file
                   on-loaded
                   on-error
                   on-loading)]
    {:play (fn []
             (.play sound))
     :stop (fn []
             (.stop sound))
     :pause (fn []
              (.pause sound))}))

(comment
  (def sound
    (load-sound {:file "/radio-show.mp3"
                 :on-loaded (fn [sound]
                              (println "LOADED!"))
                 :on-error (fn [e]
                             (println e))
                 :on-loading (fn [process]
                               (println process))}))

  ((:play sound))
  ((:stop sound))
  ((:pause sound))
  )
