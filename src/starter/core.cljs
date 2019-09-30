(ns starter.core
  (:require [reagent.core :as r]
            ["Meyda" :as m]
            ["p5" :as p5]
            ["three" :as three]))

(defonce audio-context
         (new js/AudioContext))

(defonce analytics
         (r/atom {}))

(defn audio-source []
  (.createMediaElementSource audio-context (js/document.getElementById "audio")))

(defn ->clj [typed-array]
  (js->clj
    (js/Array.apply nil
                    typed-array)))


(defn merge-function [a1 a2]
  (cond (number? a1)
        (min 1
             (+ (* 0.9 a1) (* a2 0.1)))
        (seq a1)
        (apply map (fn [item1 item2]
                     (+ (* 0.8 item1) (* item2 0.2)))
               [a1 a2])
        ))

(defn mayda-analyzer
  "Audio feature docs: https://meyda.js.org/audio-features.html"
  [audio-source]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" audio-context
                                  "source" audio-source
                                  "bufferSize" 256
                                  "featureExtractors" ["buffer"
                                                       "rms"
                                                       "powerSpectrum"
                                                       "amplitudeSpectrum"
                                                       "perceptualSharpness"]
                                  "callback" (fn [features]
                                               (let [new-analytics (-> features
                                                                       (js->clj)
                                                                       (update "amplitudeSpectrum"
                                                                               (fn [spectrum]
                                                                                 (->clj spectrum)))
                                                                       (update "powerSpectrum"
                                                                               (fn [spectrum]
                                                                                 (->clj spectrum))))]
                                                 (reset! analytics
                                                         (merge-with
                                                           merge-function
                                                           @analytics
                                                           new-analytics))))})))

(comment

  (def data [[1 2 3]
             [2 3 4]])

  (apply map (fn [& items]
               (/ (apply + items)
                  (count data)))
         data)

  )

(defn sharpness-color
  [sharpness]
  [(+ (* (- 1 sharpness) 128)
      128)
   (+ (* (- 1 sharpness) 128)
      128)
   (+ (* sharpness 128)
      128)])

(def maxi (atom 0))

(defn audio-visualizer
  "Instance mode of p5: https://github.com/processing/p5.js/wiki/Global-and-instance-mode"
  [^js sketch]
  (set! (.-setup sketch)
        (fn []
          (.createCanvas sketch 800 600)))
  (set! (.-draw sketch)
        (fn []
          (let [width (.-width sketch)
                height (.-height sketch)
                waveform (get @analytics "buffer")
                spectrum (get @analytics "powerSpectrum")
                rms (get @analytics "rms")
                sharpness (get @analytics "perceptualSharpness")]

            ;; Sharpness as background color:

            (let [[r g b] (sharpness-color sharpness)]
              (.background sketch r g b))

            ;; Visualize spectrum:
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
                spectrum))

            ;; Loudness as red circle
            (.fill sketch 255 0 0)
            (.circle sketch
                     (/ width 2)
                     (/ height 2)
                     (* rms width))

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
            (.endShape sketch)


            ))))


(defn app []
  (r/create-class
    {:component-did-mount (fn []
                            (let [audio-source (audio-source)]
                              (.connect audio-source ^js audio-context.destination)
                              (let [analyzer (mayda-analyzer audio-source)]
                                (.start analyzer)))
                            (new p5 audio-visualizer))

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
                [:span (get @analytics "perceptualSharpness")]
                ])}))

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))


