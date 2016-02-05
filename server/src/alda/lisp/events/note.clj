(ns alda.lisp.events.note
  (:require [alda.lisp.model.duration :refer (calculate-duration)]
            [alda.lisp.model.event    :refer (update-score)]
            [alda.lisp.model.offset   :refer (offset+)]
            [alda.lisp.model.records  :refer (map->Note)]
            [alda.lisp.score          :refer (update-instruments add-events)]
            [taoensso.timbre          :as    log]))

(defn- note-events
  "Given a score and a note event, returns a list of note events to be added to
   the score (one per currently active instrument).

   Each note may include an :instrument-updates key, with things to be updated
   on the instrument such as its :last- and :current-offset, as well as its
   default :duration, which is overridden if the note has an explicit duration."
  [{:keys [instruments current-instruments time-scaling time-scaling-fn
           chord-mode] :as score}
   {:keys [pitch-fn beats ms slur?] :as note}]
  (for [{:keys [id duration tempo current-offset last-offset current-marker
                quantization volume track-volume panning octave key-signature
                min-duration]
         :as inst}
        (map instruments current-instruments)]
    (let [[beats ms]         (if (or beats ms)
                               [beats ms]
                               [duration nil])
          time-scaling       (if time-scaling-fn
                               (time-scaling-fn time-scaling beats)
                               time-scaling)
          quant              (if slur? 1.0 quantization)
          full-note-duration (calculate-duration beats
                                                 tempo
                                                 time-scaling
                                                 ms)
          note-duration      (* full-note-duration quant)
          note-pitch         (pitch-fn octave key-signature)
          midi-note          (pitch-fn octave key-signature :midi true)
          note               (map->Note
                               {:offset       current-offset
                                :instrument   id
                                :volume       volume
                                :track-volume track-volume
                                :panning      panning
                                :midi-note    midi-note
                                :pitch        note-pitch
                                :duration     note-duration})
          min-duration       (when min-duration
                               (min full-note-duration min-duration))]
      (log/debug (format "%s plays at %s + %s for %s ms, at %.2f Hz."
                         id
                         current-marker
                         (int (:offset current-offset))
                         (int note-duration)
                         note-pitch))
      (assoc note :instrument-updates
             {:duration       beats
              :last-offset    (if chord-mode
                                last-offset
                                current-offset)
              :current-offset (if chord-mode
                                current-offset
                                (offset+ current-offset full-note-duration))
              :min-duration   min-duration}))))

(defmethod update-score :note
  [{:keys [beats-tally beats-tally-default instruments] :as score}
   {:keys [beats] :as note}]
  (if beats-tally
    (let [beats (or beats beats-tally-default)]
      (-> score
          (update :beats-tally + beats)
          (assoc :beats-tally-default beats)))
    (let [notes (note-events score note)
          instrument-updates (into {}
                               (map (juxt :instrument :instrument-updates)
                                    notes))]
      (-> score
          (update-instruments (merge instruments instrument-updates))
          (add-events (map #(dissoc % :instrument-updates) notes))))))

(defn note
  "Causes every instrument in :current-instruments to play a note at its
   :current-offset for the specified duration.

   If no duration is specified, the note is played for the instrument's own
   internal duration, which will be the duration last specified on a note or
   rest in that instrument's part."
  ([pitch-fn]
    (note pitch-fn nil false))
  ([pitch-fn x]
    ; x could be a duration or :slur
    (let [duration (when (map? x) x)
          slur?    (= x :slur)]
      (note pitch-fn duration slur?)))
  ([pitch-fn {:keys [beats ms slurred]} slur?]
     {:event-type :note
      :pitch-fn   pitch-fn
      :beats      beats
      :ms         ms
      :slur?      (or slur? slurred)}))

