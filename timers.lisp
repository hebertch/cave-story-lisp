(in-package :cave-story)

(defstructure timer length (ms-remaining 0))

(defun create-timer (&key length (ms-remaining 0))
  (let* ((tr (make-timer :length length :ms-remaining ms-remaining))
	 (dead?-fn (curry (compose #'not #'timer-active?) tr)))
    (def-entity-timer
	(() (update-timer tr)))
    tr))

(defun create-callback-timer (length callback-fn &optional (begin-active? t))
  (let* ((tr (make-timer :length length :ms-remaining (if begin-active?
							  length
							  0)))
	 (dead?-fn (lambda () (not (timer-active? tr)))))
    (def-entity-timer (()
		       (update-timer tr)
		       (unless (timer-active? tr)
			 (funcall callback-fn))))))

(defun create-expiring-timer (length dead?-fn &optional (begin-active? nil))
  (let ((tr (make-timer :length length :ms-remaining (if begin-active?
							 length
							 0))))
    (def-entity-timer (() (update-timer tr)))
    tr))

(defun create-looping-timer (fps tick-fn dead?-fn)
  (let ((length (fps->ms-per-frame fps)))
    (let ((tr (make-timer :length length :ms-remaining length)))
      (def-entity-timer (()
			 (update-timer tr)
			 (unless (timer-active? tr)
			   (incf (timer-ms-remaining tr) (timer-length tr))
			   (funcall tick-fn))))
      tr)))

(defun timer-active? (tr)
  (plusp (timer-ms-remaining tr)))

(defun update-timer (tr)
  (when (timer-active? tr)
    (decf (timer-ms-remaining tr) frame-time)))

(defun chunk-time-period (tm length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (mod (/ tm length-ms) chunks-per-period))

(defun chunk-timer-period (tr length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (chunk-time-period (timer-ms-remaining tr) length-ms chunks-per-period))

(defun reset-timer (tr)
  (setf (timer-ms-remaining tr) (timer-length tr)))
