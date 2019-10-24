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

(defn mayda-analyzer
  "Audio feature docs: https://meyda.js.org/audio-features.html"
  [{:keys [ctx source callback buffer-size feature-extractors]}]
  (.createMeydaAnalyzer m
                        #js {:audioContext ctx
                             :source source
                             :bufferSize buffer-size
                             :featureExtractors (clj->js feature-extractors)
                             :callback callback}))

(defn offline-analyze-sound
  "Prepares and starts the offline analyzing process.
   Feature data will be written into a state atom map `analytics-data`
   like this: {\"rms\" [0.1 0.0 1.0 ...] \"powerSpectrum\" [[1 2 3] [4 5 6] [7 8 9] ...]}"
  [sound-buffer]
  (js/Promise.
    (fn [resolve reject]
      (let [analytics-data (atom {})
            analyzer-buffer-size 512
            analyzer-feature-extractors ["rms"]
            offline-audio-context (new js/OfflineAudioContext
                                       (.-numberOfChannels sound-buffer)
                                       (.-length sound-buffer)
                                       (.-sampleRate sound-buffer))
            buffer-source (.createBufferSource offline-audio-context)
            analyzer-callback (fn [features]
                                (let [new-analytics (js->clj features)]
                                  (doseq [[key value] new-analytics]
                                    (swap! analytics-data update key
                                           (fn [analytics]
                                             (conj (or analytics [])
                                                   value))))))

            analyzer (mayda-analyzer {:ctx offline-audio-context
                                      :source buffer-source
                                      :callback analyzer-callback
                                      :buffer-size analyzer-buffer-size
                                      :feature-extractors analyzer-feature-extractors})]
        (set! (.-buffer buffer-source) sound-buffer)
        (.start buffer-source 0)
        (.start analyzer)

        (-> (.startRendering offline-audio-context)
            (.then (fn [_buffer]
                     (js/console.log "Sound analyzed")
                     (resolve @analytics-data))))))))

(defn online-analyze-sound
  "Prepares and starts the offline analyzing process.
   Feature data will be written into a state atom map `analytics-data`
   like this: {\"rms\" [0.1 0.0 1.0 ...] \"powerSpectrum\" [[1 2 3] [4 5 6] [7 8 9] ...]}"
  [{:keys [audio-ctx audio-source analyzer-callback]}]
  (let [analyzer-buffer-size 512
        analyzer-feature-extractors ["rms"]
        analyzer-callback (fn [features]
                            (let [new-analytics (js->clj features)]
                              (analyzer-callback new-analytics)))
        analyzer (mayda-analyzer {:ctx audio-ctx
                                  :source audio-source
                                  :callback analyzer-callback
                                  :buffer-size analyzer-buffer-size
                                  :feature-extractors analyzer-feature-extractors})]
    (.connect audio-source ^js audio-ctx.destination)
    (.start analyzer)))
