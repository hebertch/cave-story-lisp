(in-package :cave-story)

(defmacro setfn (function-name fn)
  "Sets the function-value of function name to be fn."
  `(setf (symbol-function ',function-name) ,fn))

;; Alist utilities
(defun alist (&rest plist)
  (plist-alist plist))

(defun aval (alist key)
  (assoc-value alist key))

(defun anorm (alist)
  (remove-duplicates alist :from-end t :key #'car))

(defun amerge (&rest alists)
  (anorm (apply #'append alists)))

(defun aset (alist &rest keys-and-vals)
  (amerge
   (loop for (k v) on keys-and-vals by #'cddr
      collecting (progn
		   (assert (typep k 'keyword))
		   (cons k v)))
   alist))
(defun asetfn (&rest keys-and-vals)
  (lambda (a)
    (apply #'aset a keys-and-vals)))

(defun arem (alist &rest keys)
  (remove-if (lambda (pair)
	       (member (car pair) keys))
	     alist))

(defun aupdate (alist &rest keys-and-fns)
  (amerge
   (loop for (k fn) on keys-and-fns by #'cddr
      collecting (cons k (funcall fn (aval alist k))))
   alist))

(defun aupdatefn (&rest keys-and-fns)
  (lambda (a) (apply #'aupdate a keys-and-fns)))

(defstruct (v2 (:conc-name nil))
  x y)

(defun make-v (x y)
  (make-v2 :x x :y y))

(defun make-rect (&key pos size)
  (alist :pos pos
	 :size size))
(defun rect-pos (rect) (aval rect :pos))
(defun rect-size (rect) (aval rect :size))

(defun tiles (tl)
  (* tl *tile-size*))
(defun tiles/2 (tl)
  (* tl (/ *tile-size* 2)))

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
  (both-v *tile-size*))
(defun tile-dims/2 ()
  (v/2 (tile-dims)))


(defun tile-pos (tp)
  (scale-v tp *tile-size*))
(defun tile-pos/2 (tp)
  (scale-v tp (/ *tile-size* 2)))

(defun tile-pos->pos (tp)
  (tile-pos tp))
(defun pos->tile-pos (p)
  (make-v (floor (x p) *tile-size*)
	  (floor (y p) *tile-size*)))

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
  "Creates a *tile-size* square at pos."
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

(defun make-cycle (&key seq (idx 0))
  (alist :seq seq
	 :idx idx))

(defun cycle-len (cycle)
  (length (aval cycle :seq)))

(defun cycle-current (c)
  (elt (aval c :seq) (aval c :idx)))

(defun cycle-next (c)
  (aset c
	:idx
	(if (= (aval c :idx) (1- (cycle-len c)))
	    0
	    (1+ (aval c :idx)))))

(defun cycle-previous (c)
  (aset c
	:idx
	(if (= (aval c :idx) 0)
	    (1- (cycle-len c))
	    (1- (aval c :idx)))))

(defun cycle-reset (c)
  (aset c :idx 0))

(defun make-timed-cycle (&key timer cycle paused?)
  (amerge
   (alist :paused? paused?
	  :update-fn #'update-timed-cycle)
   timer
   cycle))

(defun make-fps-cycle (fps seq &optional start-paused?)
  (make-timed-cycle :timer (fps-make-timer fps)
		    :cycle (make-cycle :seq seq)
		    :paused? start-paused?))

(defun update-timed-cycle (tc)
  (cond
    ((aval tc :paused?) tc)
    (t
     (multiple-value-bind (timer tick?) (timer-update tc)
       (values (if tick?
		   (cycle-next timer)
		   timer)
	       tick?)))))

(defun timed-cycle-pause (tc)
  (aset tc :paused? t))

(defun timed-cycle-resume (tc)
  (aset tc :paused? nil))

(setfn timed-cycle-restart
       (compose #'cycle-reset
		#'reset-timer))

;; Clamps

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
