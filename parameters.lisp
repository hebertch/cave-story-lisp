(in-package :cave-story)

(defvar! *fps* 60)
(defvar! *frame-time* (fps->ms-per-frame *fps*))
(defvar! *tile-size* 32)

(defvar! *debug-velocity-scale* 500)
(defvar! *window-dims* (make-v 640 480))

