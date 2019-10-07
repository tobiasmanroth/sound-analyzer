(ns starter.core
  (:require [reagent.core :as r]
            ["meyda" :as m]
            ["p5" :as p5]
            ["p5/lib/addons/p5.sound" :as p5-sound]
            ["react-p5-wrapper" :as react-p5-wrapper]))

(def react-p5
  (r/adapt-react-class react-p5-wrapper/default))

(defonce audio-context
         (new js/AudioContext))

(defn audio-source []
  (.createMediaElementSource audio-context (js/document.getElementById "audio")))

(defonce analytics
         (r/atom {}))

(defonce offline-analytics
         (r/atom []))

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
               [a1 a2])))

(defn offline-mayda-analyzer
  "Audio feature docs: https://meyda.js.org/audio-features.html"
  [ctx audio-source]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" ctx
                                  "source" audio-source
                                  "bufferSize" 256
                                  "featureExtractors" ["rms"]
                                  "callback" (fn [features]
                                               (let [new-analytics (js->clj features)]
                                                 (swap! offline-analytics
                                                        (fn [analytics]
                                                          (conj analytics new-analytics)))))})))

(defn mayda-analyzer
  "Audio feature docs: https://meyda.js.org/audio-features.html"
  [ctx audio-source]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" ctx
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

(def ready? (atom false))

(defn on-loaded [sound]
  (js/console.log "LOADED")
  (let [buffer (.-buffer sound)
        offline-audio-context (new js/OfflineAudioContext
                                   1
                                   (.-length buffer)
                                   (.-sampleRate buffer))
        buffer-source (.createBufferSource offline-audio-context)
        _ (set! (.-buffer buffer-source) buffer)
        analyzer (offline-mayda-analyzer offline-audio-context buffer-source)]
    ;;(.connect buffer-source analyzer)
    ;;(.connect analyzer (.-destination offline-audio-context))
    (.start buffer-source 0)
    (.start analyzer)

    (-> (.startRendering offline-audio-context)
        (.then (fn [buffer]
                 (js/console.log "Analysing complete!")
                 (.play sound)
                 (reset! ready? true)
                 buffer)))))


(def target-frame-rate 60)
(def frame (r/atom 0))

(defn loudness-viz
  [sketch rms]
  (let [width (.-width sketch)
        height (.-height sketch)]
    (.fill sketch 255 0 0)
    (.circle sketch
             (/ width 2)
             (/ height 2)
             (* rms width))))

(defn offline-visualizer
  "Instance mode of p5: https://github.com/processing/p5.js/wiki/Global-and-instance-mode"
  [^js sketch]
  (let [buffer-duration (atom 0)]
    (set! (.-preload sketch)
          (fn []
            (.loadSound sketch "/example.mp3" (fn [sound]
                                                (on-loaded sound)
                                                (reset! buffer-duration sound.buffer.duration)))))
    (set! (.-setup sketch)
          (fn []
            (.createCanvas sketch 400 400)))
    (set! (.-draw sketch)
          (fn []
            (when @ready?
              ;; TODO CALCULATE RIGHT BUFFER PART
              (let [percentage (/ @frame target-frame-rate @buffer-duration)
                    index (int (* percentage (count @offline-analytics)))
                    rms (get (nth @offline-analytics index)
                             "rms")]

                (.background sketch "#ffffff")

                (if (<= index (count @offline-analytics))
                  (swap! frame inc)
                  (reset! frame 0))
                (loudness-viz sketch rms)))))))

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

(defn audio-visualizer
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

(def previous-rms
  (atom (repeat 30 0)))

(defn logMap [val inMin inMax outMin outMax]
  (let [o (boolean (or (= inMax 0)
                       (= inMin 0)))
        offset (if o 1 0)
        inMin (if o (+ inMin offset) inMin)
        inMax (if o (+ inMax offset) inMax)
        a (/ (- outMin outMax) (js/Math.log10 (/ inMin inMax)))
        b (- outMin (* a (js/Math.log10 inMin)))
        r (+ (* a (js/Math.log10 (+ val offset))) b)]
    r))

(defn amplitude-over-time
  "Instance mode of p5: https://github.com/processing/p5.js/wiki/Global-and-instance-mode"
  [^js sketch]
  (set! (.-setup sketch)
        (fn []
          (doto sketch
            (.createCanvas js/window.innerWidth js/window.innerHeight)
            (.background 0)
            (.rectMode (.-CENTER sketch))
            (.colorMode (.-HSB sketch)))))
  (set! (.-draw sketch)
        (fn []
          (let [rms (get @analytics "rms")
                spacing 10
                width (.-width sketch)
                height (.-height sketch)
                w (* (/ width (* (count @previous-rms)
                                 spacing)))
                min-height 2
                roundness 20]

            (swap! previous-rms
                   (fn [rms-values]
                     (drop-last
                       (conj rms-values rms)))
                   conj rms)

            (doto sketch
              (.background "#ffffff")
              (.fill 255 10)
              (.stroke 0 0))

            (doall
              (map-indexed
                (fn [i f]
                  (let [x (.map sketch
                                i
                                (count @previous-rms)
                                0
                                width
                                (/ width 2))
                        h (.map sketch
                                f
                                0
                                0.5
                                min-height
                                height)
                        a (logMap i
                                  0
                                  (count @previous-rms)
                                  1
                                  250)
                        hueValue (.map
                                   sketch
                                   h
                                   min-height
                                   height
                                   200
                                   255)]
                    (.fill sketch hueValue 255 255 a)
                    (.rect sketch x (/ height 2) w h roundness)
                    (.rect sketch (- width x) (/ height 2) w h roundness)))
                @previous-rms))))))

(def visualizers {"v1" audio-visualizer
                  "v2" amplitude-over-time
                  "v3" offline-visualizer})

(defn app []
  (let [model (r/atom {:sketch "v3"})]
    (r/create-class
      {:component-did-mount (fn []
                              (let [audio-source (audio-source)]
                                (.connect audio-source ^js audio-context.destination)
                                (let [analyzer (mayda-analyzer audio-context audio-source)]
                                  (.start analyzer))))

       :render (fn []
                 [:div
                  [:h1 "Audio Visualizer 0.11"]
                  [:audio
                   {:controls true
                    "autoPlay" false
                    :loop true
                    "crossOrigin" "anonymous"
                    :id "audio"
                    :src "/example.mp3"}]
                  [:select
                   {:onChange (fn [e]
                                (swap! model assoc :sketch e.target.value))}
                   [:option
                    {:value "v1"}
                    "Amplitude over time"]
                   [:option
                    {:value "v2"}
                    "All"]]
                  [react-p5
                   {:sketch (get visualizers
                                 (:sketch @model))}]
                  [:span @frame]])})))

(defn stop []
  (js/console.log "Stopping..."))

(defn start []
  (js/console.log "Starting...")
  (r/render [app]
            (.getElementById js/document "app")))

(defn ^:export init []
  (start))


