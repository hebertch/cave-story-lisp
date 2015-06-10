(in-package :cave-story)

(defstruct (v2 (:conc-name nil)
	       (:constructor make-v (x y)))
  x y)

(defstruct rect
  pos
  size)

(defun tiles (tl)
  (* tl tile-size))
(defun tiles/2 (tl)
  (* tl (/ tile-size 2)))

;; Vector
(defun zero-v (&key (x 0) (y 0))
  (make-v x y))
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
(defun abs-v (a)
  (make-v (abs (x a))
	  (abs (y a))))

(defun +v (&rest vs)
  (if (null vs)
      (zero-v)
      (reduce #'add-v vs)))

(defmacro +vf (v &rest vs)
  `(setf ,v (+v ,v ,@vs)))

(defun -v (v &rest vs)
  (if (null vs)
      (sub-v (zero-v) v)
      (reduce #'sub-v vs :initial-value v)))

(defun *v (v &rest scalars)
  (scale-v v (apply #'* scalars)))

(defun v/2 (v)
  (scale-v v 1/2))
(defun tiles/2-v (x y)
  (tile-pos/2 (make-v x y)))

(defun tile-v (x y)
  (tile-pos (make-v x y)))

(defun tile-dims ()
  (both-v tile-size))
(defun tile-dims/2 ()
  (v/2 (tile-dims)))


(defun tile-pos (tp)
  (scale-v tp tile-size))
(defun tile-pos/2 (tp)
  (scale-v tp (/ tile-size 2)))

(defun tile-pos->pos (tp)
  (tile-pos tp))
(defun pos->tile-pos (p)
  (make-v (floor (x p) tile-size)
	  (floor (y p) tile-size)))

;; Rect
(defun create-rect (pos size)
  (make-rect :pos pos :size size))

(defun create-rect-cmpts (x y w h)
  (make-rect :pos (make-v x y) :size (make-v w h)))

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

(defun tile-rect (pos)
  "Creates a tile-size square at pos."
  (make-rect :pos pos :size (tile-dims)))

(defun rect-offset (rect offset-pos)
  "Moves rect by offset-pos"
  (make-rect :pos (add-v (rect-pos rect) offset-pos)
	     :size (rect-size rect)))

(defun centered-rect (pos size)
  (make-rect :pos (sub-v pos (scale-v size 1/2))
	     :size size))

(defun rects-collide? (a b)
  (and (>= (right a)  (left b))
       (<= (left a)   (right b))
       (<= (top a)    (bottom b))
       (>= (bottom a) (top b))))

;; Time
(defun s->ms (seconds) (* seconds 1000))
(defun fps->ms-per-frame (fps) (s->ms (/ 1 fps)))

;; Cycle (Animation Utilities?)

(defstruct cycle seq (idx 0) len)
(defun create-cycle (seq)
  (make-cycle :seq seq :len (length seq)))

(defun cycle-current (c)
  (elt (cycle-seq c) (cycle-idx c)))

(defun cycle-next (c)
  (incf (cycle-idx c))
  (when (= (cycle-idx c) (cycle-len c))
    (setf (cycle-idx c) 0))
  (cycle-current c))

(defun cycle-previous (c)
  (if (= (cycle-idx c) 0)
      (setf (cycle-idx c) (1- (cycle-len c)))
      (decf (cycle-idx c)))
  (cycle-current c))

(defun cycle-reset (c)
  (setf (cycle-idx c) 0))

(defstruct anim-cycle cycle timer paused?)

(defun create-anim-cycle (&key fps seq callback dead?-fn start-paused?)
  (let* ((cycle (create-cycle seq))
	 (ac (make-anim-cycle :cycle cycle
			      :paused? start-paused?)))
    (setf (anim-cycle-timer ac)
	  (create-looping-timer
	   fps
	   (lambda ()
	     (unless (anim-cycle-paused? ac)
	       (cycle-next cycle)
	       (awhen callback
		 (funcall callback cycle))))
	   dead?-fn))
    ac))

(defun anim-cycle-pause (c)
  (tf (anim-cycle-paused? c)))
(defun anim-cycle-resume (c)
  (nilf (anim-cycle-paused? c)))

(defun anim-cycle-reset (c)
  (reset-timer (anim-cycle-timer c))
  (cycle-reset (anim-cycle-cycle c)))

(defun anim-cycle-current (c)
  (cycle-current (anim-cycle-cycle c)))

(defun create-once-through-cycle (fps seq expire-fn)
  (let* (dead?
	 (cycle (create-cycle seq)))
    (create-looping-timer
     fps
     (lambda ()
       (cycle-next cycle)
       (when (zerop (cycle-idx cycle))
	 (tf dead?)
	 (funcall expire-fn)))
     (lambda () dead?))
    cycle))

;; Clamps

(defmacro clampf (number min max)
  (once-only (number)
    `(setf ,number (clamp ,number ,min ,max))))

;; TODO: 2d clamping utilities. See CAMERA-CHASE-TARGET

(defun clamp+- (val amt)
  "Clamp between +/- amt."
  (clamp val (- amt) amt))

(defun clamper+- (amt)
  (rcurry #'clamp+- amt))

(defun clamp-zero (val amt)
  "Clamps between zero and amt."
  (clamp val (min amt 0) (max amt 0)))

(defun clamper-zero (amt)
  (rcurry #'clamp-zero amt))

(defun clamp-pos (pos rect)
  (make-v (clamp (x pos) (left rect) (right rect))
	  (clamp (y pos) (top rect) (bottom rect))))

;; Random

(defun rand-val-between (min max)
  (let ((range (- max min)))
    (+ (random range) min)))

(defun rand-angle ()
  (rand-val-between (- pi) pi))
