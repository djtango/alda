(ns alda.lisp.model.records)

; attributes
(defrecord Attribute [kw-name transform-fn])

; events
(defrecord Note [offset instrument volume track-volume panning midi-note pitch duration])
(defrecord Chord [events])
(defrecord Function [offset instrument function])

; markers
(defrecord Marker [name offset])

; offset
(defrecord AbsoluteOffset [offset])
(defrecord RelativeOffset [marker offset])

