;;;; load.lisp

(ql:quickload '(:swank-tools
		:sdl
		:split-sequence))

(defparameter *file-names*
  '("package"
    "math"
    "parameters"
    "subsystems"
    "resources"
    "timers"
    "renderer"
    "input"
    "physics"
    "collisions"
    "stage"
    "player"
    "camera"
    "guns"
    "projectiles"
    "cave-story"))

(mapc #'load (mapcar (lambda (n) (concatenate 'string n ".lisp")) *file-names*))