(in-package :cave-story)

(defstruct (v2 (:conc-name nil)
	       (:constructor make-v (x y)))
  x y)

(defstruct rect
  pos
  size)

;; Vector
(defun zero-v () (make-v 0 0))
(defun add-v (a b)
  (make-v (+ (x a) (x b))
	  (+ (y a) (y b))))
(defun sub-v (a b)
  (make-v (- (x a) (x b))
	  (- (y a) (y b))))
(defun scale-v (v s)
  (make-v (* (x v) s)
	  (* (y v) s)))
(defun both-v (s)
  (make-v s s))

(defun tile-pos->pos (tp)
  (scale-v tp tile-size))
(defun pos->tile-pos (p)
  (make-v (floor (x p) tile-size)
	  (floor (y p) tile-size)))

;; Rect
(defun bottom (rect)
  (+ (y (rect-pos rect))
     (y (rect-size rect))))
(defun left (rect)
  (x (rect-pos rect)))
(defun top (rect)
  (y (rect-pos rect)))
(defun right (rect)
  (+ (x (rect-pos rect))
     (x (rect-size rect))))
(defun center (rect)
  (add-v (rect-pos rect)
	 (scale-v (rect-size rect) 1/2)))

(defun tile-rect (tile-pos)
  "Creates a tile-size square at tile-pos."
  (make-rect :pos (tile-pos->pos tile-pos)
	     :size (both-v tile-size)))
(defun rect-offset (rect offset-pos)
  "Moves rect by offset-pos"
  (make-rect :pos (add-v (rect-pos rect) offset-pos)
	     :size (rect-size rect)))

(defun centered-rect (pos size)
  (make-rect :pos (sub-v pos (scale-v size 1/2))
	     :size size))

;; Time
(defun s->ms (seconds) (* seconds 1000))
(defun fps->ms-per-frame (fps) (s->ms (/ 1 fps)))

;; Cycle (Animation Utilities?)
(defstruct cycle data current (elapsed-ms 0) ms-per-frame)

(defun create-cycle (&key fps data)
  (make-cycle :data data :current data :ms-per-frame (fps->ms-per-frame fps)))

(defun cycle-update (c)
  (incf (cycle-elapsed-ms c) frame-time)
  (when (>= (cycle-elapsed-ms c) (cycle-ms-per-frame c))
    (decf (cycle-elapsed-ms c) (cycle-ms-per-frame c))
    (aif (cdr (cycle-current c))
	 (setf (cycle-current c) it)
	 (setf (cycle-current c) (cycle-data c)))))

(defun cycle-reset (c)
  (setf (cycle-elapsed-ms c) 0)
  (setf (cycle-current c) (cycle-data c)))

;; Misc.

(defun rand-val-between (min max)
  (let ((range (- max min)))
    (+ (random range) min)))

(defmacro clampf (number min max)
  (once-only (number)
    `(setf ,number (clamp ,number ,min ,max))))
