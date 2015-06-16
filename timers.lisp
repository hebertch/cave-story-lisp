(in-package :cave-story)

(defstructure timer length (ms-remaining 0))

(defun create-expiring-timer (length &optional (begin-active? nil))
  (make-timer :length length :ms-remaining (if begin-active?
					       length
					       0)))

(defun fps-make-timer (fps &key (begin-active? t))
  (let ((len (fps->ms-per-frame fps)))
    (make-timer :length len :ms-remaining (if begin-active? len 0))))

(defun update-loop-timer (tr)
  (mvbind (tr ticked?) (update-timer tr)
    (unless (timer-active? tr)
      (incf (timer-ms-remaining tr) (timer-length tr)))
    (values tr ticked?)))

(defun timer-active? (tr)
  (plusp (timer-ms-remaining tr)))

(defun update-timer (tr)
  (let ((tr (copy-timer tr))
	ticked?)
    (when (timer-active? tr)
      (decf (timer-ms-remaining tr) frame-time)
      (unless (timer-active? tr)
	(tf ticked?)))
    (values tr ticked?)))

(defun chunk-time-period (tm length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (mod (/ tm length-ms) chunks-per-period))

(defun chunk-timer-period (tr length-ms &optional (chunks-per-period 2))
  "Chunks the time remaining into chunks of LENGTH-MS. Returns the idx of the chunk in the period."
  (chunk-time-period (timer-ms-remaining tr) length-ms chunks-per-period))

(defun reset-timer (tr)
  (let ((tr (copy-timer tr)))
    (setf (timer-ms-remaining tr) (timer-length tr))
    tr))
