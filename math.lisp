(in-package :cave-story)

(defmacro setfn (function-name fn-or-doc &rest fns)
  "Sets the function-value of function name to be the COMP
of fn and fns."
  (if (stringp fn-or-doc)
      (progn
	(assert (first fns))
	`(setf (documentation ',function-name 'function) ,fn-or-doc
	       (fdefinition ',function-name) (comp ,@fns)))
      `(setf (fdefinition ',function-name) (comp ,fn-or-doc ,@fns))))

(defun expand-partial-application (lst)
  "Expand the body forms in lst into a partial application. _'s in the
top level forms are replaced with arguments."
  (let* ((rest? (not (null (cdr (last lst)))))
	 (lst (if rest?
		  (append (butlast lst) (list (car (last lst))
					      (cdr (last lst))))
		  lst))
	 (num-args (count '_ lst))
	 (args (loop for n from 1 to num-args
		  collecting (gensym (format nil "A~A-" n))))
	 (arg-list (loop
		      for a in (rest lst)
		      with i = 0
		      collecting
			(if (eq '_ a)
			    (progn
			      (incf i)
			      (elt args (1- i)))
			    a))))
    `(lambda ,args
       (,@(if rest?
	      `(apply #',(first lst))
	      (list (first lst)))
	  ,@arg-list))))

(defun expand-composition (lst)
  "Given a list of forms, lst, expand into function composition.
Replace symbols with their function slots."
  `(lambda (arg)
     (funcall (compose
	       ,@(loop for i in lst
		    collecting (if (symbolp i)
				   `(function ,i)
				   i)))
	      arg)))

(defun hash-underscore-reader (stream char arg)
  "Reader for #_. Expands to partial application using PART."
  (declare (ignore char arg))
  `(part ,(read stream t nil t)))

(defun install-function-syntax! ()
  (set-dispatch-macro-character #\# #\_ #'hash-underscore-reader)
  :done)
(install-function-syntax!)

(defmacro part (&body forms)
  "Partial function application. _'s are converted to args.
E.g. (+ _ A _) ==> (lambda (#:a1 #:a2 #:a3) (+ #:a1 A #:a2)).
Args cannot be referenced."
  (apply #'expand-partial-application forms))

(defmacro comp (&rest forms)
  "Function composition. If a form is a symbol, its function value is used.
Use COMPOSE when a function is stored in the value slot."
  (expand-composition forms))

(defmacro defvar* (var &optional (val nil val-provided?) doc)
  "If no val is provided, make var unbound, and then defvar it.
If val is provided, expand to a defparameter."
  (if val-provided?
      (list* 'defparameter var val (if doc (list doc) nil))
      `(progn
	 (makunbound ',var)
	 (defvar ,var))))

;; Alist utilities
(defun alist (&rest plist)
  (plist-alist plist))

(defun aval (alist key)
  (assoc-value alist key))

(defun avalfn (key)
  #_(aval _ key))

(defun anorm (alist)
  (remove-duplicates alist :from-end t :key #'car))

(defun amerge (&rest alists)
  (anorm (apply #'append alists)))


(deftype keytype ()
  '(or integer symbol))

(defun aset (alist &rest keys-and-vals)
  (assert (listp alist))
  (amerge
   (loop for (k v) on keys-and-vals by #'cddr
      collecting (progn
		   (assert (typep k 'keytype))
		   (cons k v)))
   alist))

(defun asetfn (&rest keys-and-vals)
  #_(aset _ . keys-and-vals))

(defun arem (alist &rest keys)
  (assert (listp alist))
  (remove-if (lambda (pair)
	       (member (car pair) keys))
	     alist))

(defun aremfn (&rest keys)
  #_(arem _ . keys))

(defun call-if (fn val &optional default-fn)
  "Calls fn on val if fn is not null, otherwise returns val.
If fn is null and default is provided, return (funcall default val)."
  (if fn
      (funcall fn val)
      (if default-fn
	  (funcall default-fn val)
	  val)))

(defun aupdate (alist &rest keys-and-fns)
  "If a function is NIL, then the new value is the same as the old."
  (assert (listp alist))
  (amerge
   (loop for (k fn) on keys-and-fns by #'cddr
      collecting (progn
		   (assert (typep k 'keytype))
		   (cons k (call-if fn (aval alist k)))))
   alist))

(defun akeys (alist)
  "Return the keys of alist."
  (mapcar #'car alist))
(defun avals (alist)
  "Return the values of alist."
  (mapcar #'cdr alist))

(defun aupdatefn (&rest keys-and-fns)
  #_(aupdate _ . keys-and-fns))

(defun pushfn (&rest vals)
  #_(append vals _))
(defun adjoinfn (&rest vals)
  #_(union _ vals))
(defun appendfn (&rest lsts)
  #_(append _ . lsts))
(defun removefn (&rest vals)
  "Returns a function to remove all vals from a list."
  #_(set-difference _ vals))

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

(defun v2= (v1 &rest vs)
  (if (null vs)
      t
      (let ((v2 (first vs)))
	(and (= (x v1) (x v2))
	     (= (y v1) (y v2))
	     (apply 'v2= v1 (rest vs))))))

(defun *v (v &rest scalars)
  (scale-v v (apply #'* scalars)))

(defun v/2 (v)
  (/ v 2))
(defun tiles/2-v (x y)
  (tile-pos/2 (make-v x y)))

(defun tile-v (x y)
  (tile-pos (make-v x y)))

(defun tile-dims ()
  (both-v *tile-size*))
(defun tile-dims/2 ()
  (v/2 (tile-dims)))


(defun tile-pos (tp)
  (* tp *tile-size*))
(defun tile-pos/2 (tp)
  (* tp (/ *tile-size* 2)))

(defun tile-pos->pos (tp)
  (tile-pos tp))
(defun pos->tile-pos (p)
  (make-v (floor (x p) *tile-size*)
	  (floor (y p) *tile-size*)))

(defun magnitude (v)
  (sqrt (+ (expt (x v) 2)
	   (expt (y v) 2))))

(defun dist (va vb)
  (let ((disp (- va vb)))
    (magnitude disp)))

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
	 (* (rect-size rect) 1/2)))

(defun tile-rect (pos)
  "Creates a *tile-size* square at pos."
  (make-rect :pos pos :size (tile-dims)))

(defun rect-offset (rect offset-pos)
  "Moves rect by offset-pos"
  (make-rect :pos (add-v (rect-pos rect) offset-pos)
	     :size (rect-size rect)))

(defun rect-from-two-points (a b)
  (let ((xmin (min (x a) (x b)))
	(ymin (min (y a) (y b)))
	(xmax (max (x a) (x b)))
	(ymax (max (y a) (y b))))
    (make-rect :pos (make-v xmin ymin)
	       :size (make-v (- ymax ymin)
			     (- xmax xmin)))))

(defun centered-rect (pos size)
  (make-rect :pos (- pos (* size 1/2))
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
       cycle-reset reset-timer)

;; Clamps

(defun clamp+- (val amt)
  "Clamp between +/- amt."
  (clamp val (- amt) amt))

(defun clamper+- (amt)
  #_(clamp+- _ amt))

(defun clamp-zero (val amt)
  "Clamps between zero and amt."
  (clamp val (min amt 0) (max amt 0)))

(defun clamper-zero (amt)
  #_(clamp-zero _ amt))

(defun clamp-pos (pos rect)
  (make-v (clamp (x pos) (left rect) (right rect))
	  (clamp (y pos) (top rect) (bottom rect))))

;; Random

(defun rand-val-between (min max)
  (let ((range (- max min)))
    (+ (random range) min)))

(defun rand-angle ()
  (rand-val-between (- pi) pi))

(defmacro rolling-average-time (param &body body)
  "Stores and retrieves rolling average data in and from param.
Pushes the time-delta for body onto the rolling average.
Returns the value from body."
  (with-gensyms (val start-time)
    `(let (,val
	   (,start-time (sdl:get-ticks)))
       (setq ,val (progn ,@body))
       (setq ,param (push-time-delta ,param (- (sdl:get-ticks) ,start-time)))
       ,val)))

(defun make-rolling-average (num-entries)
  (alist :num-entries num-entries))

(defun push-time-delta (time-data delta)
  "Push a time delta onto a rolling average"
  (let ((times (aval time-data :times))
	(num-entries (aval time-data :num-entries)))
    (if (< (length times) num-entries)
	(aupdate time-data
		 :times (pushfn delta))
	(aupdate time-data
		 :times (lambda (times)
			  (cons delta (butlast times)))))))
(defun rolling-average (time-data)
  "Calculate the average from a rolling average."
  (let ((total 0)
	(len (length (aval time-data :times))))
    (dolist (time (aval time-data :times))
      (setq total (+ total time)))
    (if (zerop len)
	0
	(/ total len))))

(defun rolling-average-percent (time-data)
  "Return the rolling average as a percentage of the frame-time."
  (let ((avg (rolling-average time-data)))
    (* 100.0 (/ avg *frame-time*))))

(defun + (&rest numbers)
  (if (and (not (null numbers)) (typep (first numbers) 'v2))
      (apply '+v (first numbers) (rest numbers))
      (apply 'cl:+ numbers)))

(defun - (number &rest numbers)
  (typecase number
    (v2 (apply '-v number numbers))
    (t (apply 'cl:- number numbers))))

(defun * (&rest numbers)
  (let ((v (find-if (lambda (n) (typep n 'v2)) numbers)))
    (if v
	(scale-v v (apply 'cl:* (remove v numbers)))
	(apply 'cl:* numbers))))

(defun / (number &rest numbers)
  (if (typep number 'v2)
      (scale-v number (apply 'cl:/ 1 numbers))
      (apply 'cl:/ number numbers)))

(defun = (number &rest numbers)
  (if (typep number 'v2)
      (apply 'v2= number numbers)
      (apply 'cl:= number numbers)))

(defun abs (number)
  (typecase number
    (v2 (abs-v number))
    (t (cl:abs number))))

(defun keyword-destructure-form? (destructure-form)
  (keywordp destructure-form))

(defun symbol-destructure-form? (destructure-form)
  (symbolp destructure-form))

(defun dispatch-destructure-form? (destructure-form)
  (and (listp destructure-form)
       (not (null destructure-form))
       (keywordp (first destructure-form))
       (find (first destructure-form) (akeys *destructurers*))))

(defun destructure-alist (form val-name)
  "Destructures an alist.
Form is a plist of (:key destructure-form  ...)
Omit a key/destructure-form pair to avoid binding."
  (labels
      ((aux (form val-name)
	 (cond
	   ((null form) nil)
	   (t
	    (let* ((key (first form))
		   (destructure-form (second form))
		   (val-form (list 'aval val-name key)))
	      (if (symbol-destructure-form? destructure-form)
		  (append (destructure (list destructure-form val-form))
			  (aux (rest (rest form)) val-name))
		  (let ((inner-val-name (gensym (string key))))
		    (append (list (list inner-val-name val-form))
			    (destructure (list destructure-form inner-val-name))
			    (aux (rest (rest form)) val-name)))))))))
    (aux form val-name)))

(defvar* *destructurers*
    (alist :alist 'destructure-alist)
  "Alist of (destructure-key . destructure-fn).
destructure-fn of the form (destructure-fn form val-name).")

(defun dispatch-destructure (structure-key form val-name)
  "Destructure based on the structure-key."
  (let ((destructurer (aval *destructurers* structure-key)))
    (funcall destructurer form val-name)))

(defun destructure-list-form (list-form val-name)
  "Destructures a list (dotted or no) of destructure-forms."
  (labels ((aux (cons-form val-name)
	     (cond
	       ((null cons-form) nil)
	       ((listp (cdr cons-form))
		(let* ((inner-val-name (gensym))
		       (bindings (aux (cdr cons-form) inner-val-name)))
		  (if bindings
		      (append (destructure (list (car cons-form) (list 'car val-name)))
			      (cons (list inner-val-name (list 'cdr val-name))
				    bindings))
		      (destructure (list (car cons-form) (list 'car val-name))))))
	       (t
		(append (destructure (list (car cons-form) (list 'car val-name)))
			(destructure (list (cdr cons-form) (list 'cdr val-name))))))))
    (aux list-form val-name)))

(defun destructure (binding-form)
  "Returns a list of destructured binding-forms.
Binding-form is a list of (destructure-form value-form).
destructure-form: symbol |
                  list |
                  (structure-keyword structure...)"
  (if (symbolp binding-form)
      ;; Binding-form (including val-form) is just a symbol. leave it.
      (list binding-form)
      (let ((destructure-form (first binding-form))
	    (val-form (second binding-form)))
	(cond
	  ((keyword-destructure-form? destructure-form)
	   ;; Keywords are ignored.
	   nil)
	  ((symbol-destructure-form? destructure-form)
	   (list binding-form))
	  ((dispatch-destructure-form? destructure-form)
	   ;; Structure form. dispatch.
	   (if (symbolp val-form)
	       ;; Don't create a new binding if it's already a symbol.
	       ;; Reduces clutter of generated code.
	       (dispatch-destructure (first destructure-form) (rest destructure-form) val-form)
	       (let ((val-name (gensym (string (first destructure-form)))))
		 (cons (list val-name val-form)
		       (dispatch-destructure (first destructure-form) (rest destructure-form) val-name)))))
	  (t
	   ;; List form.
	   (if (symbolp val-form)
	       (destructure-list-form destructure-form val-form)
	       (let ((val-name (gensym)))
		 (cons (list val-name val-form)
		       (destructure-list-form destructure-form val-name)))))))))

(defmacro dlet* ((&rest binding-forms) &body forms)
  "Destructuring macro. Destructures using destructure."
  (let ((destructured-bindings (mapcan #'destructure binding-forms)))
    `(let* ,destructured-bindings
       ,@forms)))

(defmacro dbind (destructure-form value-form &body forms)
  `(dlet* ((,destructure-form ,value-form))
     ,@forms))

(defmacro when-dlet* ((&rest binding-forms) &body forms)
  "Destructures binding-forms and uses them in when-let*.
If any binding is nil, when-dlet* short-circuits and returns nil.
Forms executed with the bindings."
  (let ((destructured-bindings (mapcan #'destructure binding-forms)))
    `(when-let* ,destructured-bindings
       ,@forms)))
