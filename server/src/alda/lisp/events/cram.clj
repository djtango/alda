(ns alda.lisp.events.cram
  (:require [alda.lisp.attributes  :refer  (set-duration)]
            [alda.lisp.model.event :refer (update-score add-events)]))

(defn tally-beats
  [score events]
  (-> score
      (assoc :beats-tally 0
             :beats-tally-default 1)
      (add-events events)
      :beats-tally))

(defn calculate-time-scaling
  "Given a :time-scaling value, the 'inner' length of a cram in beats, and the
   'outer' length of the cram in beats, calculates the effective time-scaling
   value."
  [time-scaling inner-beats outer-beats]
  (* (/ time-scaling inner-beats) outer-beats))

(defmethod update-score :cram
  [{:keys [time-scaling] :as score}
   {:keys [duration events] :as cram}]
  (let [inner-beats     (tally-beats score events)
        time-scaling-fn (fn [time-scaling outer-beats]
                          (calculate-time-scaling time-scaling
                                                  inner-beats
                                                  outer-beats))]
    (-> score
        (assoc :time-scaling-fn time-scaling-fn)
        (add-events events)
        (assoc :time-scaling-fn nil))))

(defn cram
  "A cram expression evaluates the events it contains, time-scaled based on the
   inner tally of beats in the events and the outer durations of each current
   instrument."
  [& events]
  (let [[duration events] (if (:duration? (last events))
                            (cons (last events) (butlast events))
                            (cons nil events))]
    {:event-type :cram
     :duration   duration
     :events     events}))

