(in-package :cave-story)

(defstruct timer length (ms-remaining 0) looping?)

(defun create-expiring-timer (length &optional (begin-active? nil))
  (make-timer :length length
	      :ms-remaining (if begin-active?
				length
				0)
	      :looping? nil))

(defun fps-make-timer (fps &key (begin-active? t))
  (let ((len (fps->ms-per-frame fps)))
    (make-timer :length len :ms-remaining (if begin-active? len 0) :looping? t)))

(defun timer-active? (tr)
  (and tr (plusp (timer-ms-remaining tr))))
(defun timer-expired? (tr)
  (not (timer-active? tr)))

(defgeneric update-timer (tr))

(defun timer-update (tr)
  ;; TODO: Admittedly the naming here sucks.
  (cond ((timer-active? tr)
	 (let* ((tr2 (make-timer :length (timer-length tr)
				 :ms-remaining (- (timer-ms-remaining tr) *frame-time*)
				 :looping? (timer-looping? tr)))
		(ticked? (not (timer-active? tr2))))
	   (values
	    (cond ((and ticked? (timer-looping? tr2))
		   (make-timer :length (timer-length tr2)
			       :ms-remaining (+ (timer-ms-remaining tr2)
						(timer-length tr2))
			       :looping? (timer-looping? tr2)))
		  (t tr2))
	    ticked?)))
	(t tr)))

(defmethod update-timer ((tr timer))
  (timer-update tr))

(defun chunk-time-period (tm length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (mod (/ tm length-ms) chunks-per-period))

(defun chunk-timer-period (tr length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (chunk-time-period (timer-ms-remaining tr) length-ms chunks-per-period))

(defun reset-timer (tr)
  (let ((tr (copy-structure tr)))
    (setf (timer-ms-remaining tr) (timer-length tr))
    tr))

(defun timer-set-update (ts)
  (let ((ticks))
    (values (mapcar
	     (lambda (pair)
	       (let ((k (car pair))
		     (v (cdr pair)))
		 (mvbind (tr tick?) (update-timer v)
		   (when tick?
		     (push k ticks))
		   (cons k tr))))
	     ts)
	    ticks)))

(defmacro timer-set-updatef (ts)
  `(setf ,ts (timer-set-update ,ts)))
