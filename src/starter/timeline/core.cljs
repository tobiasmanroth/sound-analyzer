(ns starter.timeline.core)

;; Concept:
;; From `storrito.editor-timeline.core`
;;
;; The timeline is the mechanism that controls the complete playback
;; of the story in the editor. The playback comprises different
;; components like videos, gif-videos, gif-sticker and animations. All
;; of them needs to be played synchronously. This namespace includes
;; tools to accomplish this.

(defn play-loop
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
        (let [new-state-value (swap! state (fn [state-value]
                                             (f (assoc state-value
                                                  :time/now
                                                  (js/performance.now)))))]
          (js/requestAnimationFrame raf))))))

(defn time-delta
  "Adds the `:time/delta` to the `state-value` that is the milliseconds
   which have passed, since the last rendering (usually 16.66 ms due to
   the browser's FPS of 60)."
  [state-value]
  (assoc state-value
    :time/delta (if-not (:time/now* state-value)
                  0
                  (- (:time/now state-value)
                     (:time/now* state-value)))
    ;; stores the current `:time/now` to calculate the next
    ;; `:time/delta`:
    :time/now* (:time/now state-value)))

(defn time-current
  "Calculates the time that has passed, since the playback of the
   timeline has started. It is stored as `:time/current` (unit ms) in
   the `state-value`."
  [state-value]
  (let [time-current* (:time/current state-value
                        0)]
    (assoc state-value
      :time/current (+ time-current*
                       (:time/delta state-value))
      ;; provides the previous `:time/current` as
      ;; `:time/current*` to the subsequent functions, so that
      ;; they can trigger an action at a desired time. If a
      ;; function likes to start an animation at the
      ;; `desired-time` 2000 ms, the rendering most likely hits
      ;; timestamps around this trigger. The function can then
      ;; use the check `(<= (:time/current* state-value)
      ;; desired-time (:time/current state-value))` to start the
      ;; animation:
      :time/current* time-current*)))

(defn stop-at
  "Stops the complete timeline, when the `:time/current` has passed the
   `:time/stop-at`."
  [state-value]
  (if (and (:time/stop-at state-value)
           (>= (:time/current state-value)
               (:time/stop-at state-value)))
    (assoc state-value
      :time/stopped true)
    state-value))

(defn start-video
  "Starts the HTML `video` as soon as the `start-at` timestamp is
   reached."
  [{:keys [video start-at]}]
  (fn [state-value]
    (let [video-playing (<= (or start-at
                                0)
                            (:time/current state-value))]
      (when (and video-playing
                 (.-paused video))
        (.play video))
      state-value)))

(defn stop-video
  "Stops the HTML `video` as soon as the `stop-at` timestamp is
   reached."
  [{:keys [video stop-at]}]
  (fn [state-value]
    (let [video-stopped (or (and stop-at
                                 (<= stop-at
                                     (:time/current state-value)))
                            (:time/stopped state-value))]
      (when (and video-stopped
                 (not (.-paused video)))
        (.pause video))
      state-value)))

(defn play-video
  "Combines `start-video` and `stop-video`."
  [params]
  (comp (stop-video params)
        (start-video params)))

(defn draw-video
  "Draws the `video` on the `canvas`."
  [{:keys [video canvas]}]
  (fn [state-value]
    (let [ctx (.getContext canvas "2d")]
      (.drawImage ctx video 0 0 (.-width canvas) (.-height canvas)))
    state-value))

(defn prn-state
  "Helper function to print the current state value at every frame."
  [state-value]
  (prn state-value)
  state-value)
