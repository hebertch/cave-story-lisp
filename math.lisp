(in-package :cave-story)

(defstruct (v2 (:conc-name nil))
  x y)

(defun make-v (x y)
  (make-v2 :x x :y y))

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

(defun dist (va vb)
  (let ((disp (-v va vb)))
    (sqrt (+ (expt (x disp) 2)
	     (expt (y disp) 2)))))

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

(defstruct cycle seq (idx 0))

(defun cycle-len (cycle)
  (length (cycle-seq cycle)))

(defun create-cycle (seq)
  (make-cycle :seq seq))

(defun cycle-current (c)
  (elt (cycle-seq c) (cycle-idx c)))

(defun cycle-next (c)
  (make-cycle
   :seq (cycle-seq c)
   :idx
   (if (= (cycle-idx c) (1- (cycle-len c)))
       0
       (1+ (cycle-idx c)))))

(defun cycle-previous (c)
  (make-cycle
   :seq (cycle-seq c)
   :idx
   (if (= (cycle-idx c) 0)
       (1- (cycle-len c))
       (1- (cycle-idx c)))))

(defun cycle-reset (c)
  (make-cycle
   :seq (cycle-seq c)
   :idx 0))

(defstruct timed-cycle
  timer
  cycle
  paused?)

(defun create-timed-cycle (fps seq &optional start-paused?)
  (make-timed-cycle :timer (fps-make-timer fps)
		    :cycle (create-cycle seq)
		    :paused? start-paused?))

(defmethod update-timer ((tc timed-cycle))
  (cond
    ((timed-cycle-paused? tc) tc)
    (t
     (mvbind (timer tick?) (update-timer (timed-cycle-timer tc))
       (values (make-timed-cycle
		:timer timer
		:cycle (if tick?
			   (cycle-next (timed-cycle-cycle tc))
			   (timed-cycle-cycle tc))
		:paused? nil)
	       tick?)))))

(defun timed-cycle-current (tc)
  (cycle-current (timed-cycle-cycle tc)))

(defun timed-cycle-pause (tc)
  (make-timed-cycle
   :timer (timed-cycle-timer tc)
   :cycle (timed-cycle-cycle tc)
   :paused? t))

(defun timed-cycle-resume (tc)
  (make-timed-cycle
   :timer (timed-cycle-timer tc)
   :cycle (timed-cycle-cycle tc)
   :paused? nil))

(defun timed-cycle-restart (tc)
  (make-timed-cycle
   :timer (reset-timer (timed-cycle-timer tc))
   :cycle (cycle-reset (timed-cycle-cycle tc))
   :paused? (timed-cycle-paused? tc)))

;; Clamps

(defmacro clampf (number min max)
  (once-only (number)
    `(setf ,number (clamp ,number ,min ,max))))

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
