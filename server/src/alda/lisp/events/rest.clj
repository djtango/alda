(ns alda.lisp.events.rest
  (:require [alda.lisp.model.duration :refer (duration calculate-duration)]
            [alda.lisp.model.event    :refer (update-score)]
            [alda.lisp.model.offset   :refer (offset+)]
            [taoensso.timbre          :as    log]))

(defmethod update-score :rest
  [{:keys [beats-tally beats-tally-default current-instruments time-scaling
           time-scaling-fn chord-mode] :as score}
   {:keys [beats ms] :as rest-event}]
  (if beats-tally
    (let [beats (or beats beats-tally-default)]
      (-> score
          (update :beats-tally + beats)
          (assoc :beats-tally-default beats)))
    (update score :instruments
            into {}
            map (fn [[id {:keys [tempo last-offset current-offset current-marker
                                 duration min-duration]
                          :as inst}]]
                  (if (contains? current-instruments id)
                    (let [[beats ms]    (if (or beats ms)
                                          [beats ms]
                                          [duration nil])
                          time-scaling  (if time-scaling-fn
                                          (time-scaling-fn time-scaling beats)
                                          time-scaling)
                          rest-duration (calculate-duration beats
                                                            tempo
                                                            time-scaling
                                                            ms)
                          min-duration  (when min-duration
                                          (min rest-duration min-duration))]
                      (log/debug (format "%s rests at %s + %s for %s ms."
                                         id
                                         current-marker
                                         (int (:offset current-offset))
                                         (int rest-duration)))
                      [id (assoc inst
                                 :duration       beats
                                 :last-offset    (if chord-mode
                                                   last-offset
                                                   current-offset)
                                 :current-offset (if chord-mode
                                                   current-offset
                                                   (offset+ current-offset
                                                            rest-duration))
                                 :min-duration   min-duration)])
                    [id inst])))))

(defn pause
  "Causes every instrument in :current-instruments to rest (not play) for the
   specified duration.

   If no duration is specified, each instrument will rest for its own internal
   duration, which will be the duration last specified on a note or rest in
   that instrument's part."
  [& [{:keys [beats ms] :as dur}]]
   {:event-type :rest
    :beats      beats
    :ms         ms})

