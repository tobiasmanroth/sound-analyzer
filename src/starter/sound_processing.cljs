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
  [{:keys [ctx source callback buffer-size feature-extractors]}]
  (.createMeydaAnalyzer m
                        (clj->js {"audioContext" ctx
                                  "source" source
                                  "bufferSize" buffer-size
                                  "featureExtractors" feature-extractors
                                  "callback" callback})))

(defn analyze-sound
  "Prepares and starts the offline analyzing process.
   Feature data will be written into a state atom map `analytics-data`
   like this: {\"rms\" [0.1 0.0 1.0 ...] \"powerSpectrum\" [[1 2 3] [4 5 6] [7 8 9] ...]}"
  [{:keys [sound on-analyzed on-analyzing]}]
  (let [analytics-data (atom {})
        analyzer-buffer-size 256
        analyzer-feature-extractors ["rms"]
        buffer (.-buffer sound)
        offline-audio-context (new js/OfflineAudioContext
                                   2
                                   (.-length buffer)
                                   (.-sampleRate buffer))
        buffer-source (.createBufferSource offline-audio-context)

        analyzer-callback (fn [features]
                            (let [new-analytics (js->clj features)]
                              (doseq [[key value] new-analytics]
                                (swap! analytics-data update key
                                       (fn [analytics]
                                         (conj (or analytics [])
                                               value))))
                              (when on-analyzing
                                (let [progress (/ (count @analytics-data)
                                                  (/ (.-length buffer) analyzer-buffer-size))]
                                  (on-analyzing progress)))))

        analyzer (offline-mayda-analyzer {:ctx offline-audio-context
                                          :source buffer-source
                                          :callback analyzer-callback
                                          :buffer-size analyzer-buffer-size
                                          :feature-extractors analyzer-feature-extractors})]
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