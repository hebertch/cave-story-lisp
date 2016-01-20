(in-package :cave-story)

(defun make-timer (&key length (ms-remaining 0) looping?)
  (alist :length length
	 :ms-remaining ms-remaining
	 :looping? looping?
	 :update-fn #'timer-update))

(defun make-expiring-timer (length &optional (begin-active? nil))
  (make-timer :length length
	      :ms-remaining (if begin-active?
				length
				0)
	      :looping? nil))

(defun fps-make-timer (fps &key (begin-active? t))
  (let ((len (fps->ms-per-frame fps)))
    (make-timer :length len :ms-remaining (if begin-active? len 0) :looping? t)))

(defun timer-active? (tr)
  (and tr (plusp (aval tr :ms-remaining))))
(defun timer-expired? (tr)
  (not (timer-active? tr)))

(defun timer-update (tr)
  (cond ((timer-active? tr)
	 (let* ((tr2 (aset tr
			   :ms-remaining (- (aval tr :ms-remaining)
					    *frame-time*)))
		(ticked? (not (timer-active? tr2))))
	   (values
	    (cond ((and ticked? (aval tr2 :looping?))
		   (aset tr2
			 :ms-remaining (+ (aval tr2 :ms-remaining)
					  (aval tr2 :length))))
		  (t tr2))
	    ticked?)))
	(t tr)))

(defun chunk-time-period (tm length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (mod (/ tm length-ms) chunks-per-period))

(defun chunk-timer-period (tr length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (chunk-time-period (aval tr :ms-remaining) length-ms chunks-per-period))

(defun reset-timer (tr)
  (aset tr :ms-remaining (aval tr :length)))

(defun timer-set-update (o)
  (let ((ts (aval o :timers)))
    (if (consp (first ts))
	(let* ((ticks)
	       (timers
		(mapcar
		 (lambda (pair)
		   (let ((k (car pair))
			 (v (cdr pair)))
		     (when v
		       (multiple-value-bind (tr tick?) (update-timer v)
			 (when tick?
			   (push k ticks))
			 (cons k tr)))))
		 ts)))
	  (aset o :timers timers :ticks ticks))
	(progn
	  (dolist (k ts)
	    (let ((v (aval o k)))
	      (when v
		(multiple-value-bind (tr tick?) (update-timer v)
		  (setq o
			(aset o
			      k tr
			      :ticks (if tick?
					 (cons k (aval o :ticks))
					 (aval o :ticks))))))))
	  o))))
