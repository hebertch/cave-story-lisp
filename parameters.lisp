(in-package :cave-story)

(defparameter *fps* 60)
(defparameter *frame-time* (fps->ms-per-frame *fps*))
(defparameter *tile-size* 32)

(defparameter *debug-velocity-scale* 500)
(defparameter *window-dims* (make-v 640 480))

