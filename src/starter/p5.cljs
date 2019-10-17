(ns starter.p5
  (:require [reagent.core :as r]
            ["p5" :as p5]
            ["meyda" :as m]
            ["p5/lib/addons/p5.sound" :as p5-sound]
            ["react-p5-wrapper" :as react-p5-wrapper]))

(def react-p5
  (r/adapt-react-class react-p5-wrapper/default))

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

(defn p5-sketch
  "TODO"
  [sketch-map]
  (fn [^js p5-sketch-object]
    (when-let [preload-fn (:preload sketch-map)]
      (set! (.-preload p5-sketch-object)
            (partial preload-fn p5-sketch-object)))

    (when-let [setup-fn (:setup sketch-map)]
      (set! (.-setup p5-sketch-object)
            (partial setup-fn p5-sketch-object)))

    (when-let [draw-fn (:draw sketch-map)]
      (set! (.-draw p5-sketch-object)
            (partial draw-fn p5-sketch-object)))))

(defn react-p5-sketch
  [sketch-map]
  [react-p5
   {:sketch (p5-sketch sketch-map)}])

(defn load-sound
  [{:keys [file on-loaded on-error on-loading]}]
  (js/Promise.
    (fn [resolve reject]
      (new (.-SoundFile p5)
           file
           (fn [sound]
             (js/console.log "Sound loaded")
             (resolve sound))
           on-error
           on-loading))))

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
