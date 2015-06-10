(in-package :cave-story)

(defstruct timer length (ms-remaining 0))

(defun create-timer (&key length (ms-remaining 0))
  (let ((tr (make-timer :length length :ms-remaining ms-remaining)))
    (register-timer :dead?-fn (curry (compose #'not #'timer-active?) tr)
		    :update-fn (curry #'update-timer tr))
    tr))

(defun create-expiring-timer (length dead?-fn &optional (begin-active? nil))
  (let ((tr (make-timer :length length :ms-remaining (if begin-active?
							 length
							 0))))
    (register-timer :update-fn (curry #'update-timer tr)
		    :dead?-fn dead?-fn)
    tr))

(defun create-looping-timer (fps tick-fn dead?-fn)
  (let ((length (fps->ms-per-frame fps)))
    (let ((tr (make-timer :length length :ms-remaining length)))
      (register-timer :update-fn
		      (lambda ()
			(update-timer tr)
			(unless (timer-active? tr)
			  (incf (timer-ms-remaining tr) (timer-length tr))
			  (funcall tick-fn)))
		      :dead?-fn dead?-fn)
      tr)))

(defun timer-active? (tr)
  (plusp (timer-ms-remaining tr)))

(defun update-timer (tr)
  (when (timer-active? tr)
    (decf (timer-ms-remaining tr) frame-time)))

(defun chunk-time-period (tr length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (mod (/ (timer-ms-remaining tr) length-ms) chunks-per-period))

(defun reset-timer (tr)
  (setf (timer-ms-remaining tr) (timer-length tr)))
