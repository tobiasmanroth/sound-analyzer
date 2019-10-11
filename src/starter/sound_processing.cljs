(ns starter.sound-processing
  (:require ["meyda" :as m]))

(def analyzer-buffer-size 256)

(def analytics
  (atom []))

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
  [{:keys [ctx source callback buffer-size]}]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" ctx
                                  "source" source
                                  "bufferSize" buffer-size
                                  "featureExtractors" ["rms"]
                                  "callback" callback})))

(defn analyze-sound
  "Prepares and starts the offline analyzing process."
  [{:keys [sound on-analyzed on-analyzing]}]
  (let [analytics-data (atom [])
        buffer-size 256
        buffer (.-buffer sound)
        offline-audio-context (new js/OfflineAudioContext
                                   2
                                   (.-length buffer)
                                   (.-sampleRate buffer))
        buffer-source (.createBufferSource offline-audio-context)
        analyzer-callback (fn [features]
                            (let [new-analytics (js->clj features)]
                              (swap! analytics-data
                                     (fn [analytics]
                                       (conj analytics new-analytics)))
                              (when on-analyzing
                                (let [progress (/ (count @analytics-data)
                                                  (/ (.-length buffer) buffer-size))]
                                  (on-analyzing progress)))))
        analyzer (offline-mayda-analyzer {:ctx offline-audio-context
                                          :source buffer-source
                                          :callback analyzer-callback
                                          :buffer-size buffer-size})]
    (set! (.-buffer buffer-source) buffer)
    (.start buffer-source 0)
    (.start analyzer)

    (-> (.startRendering offline-audio-context)
        (.then (fn [_buffer]
                 (on-analyzed @analytics-data))))))

(defn mayda-analyzer
  "Audio feature docs: https://meyda.js.org/audio-features.html"
  [ctx audio-source]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" ctx
                                  "source" audio-source
                                  "bufferSize" analyzer-buffer-size
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