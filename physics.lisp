(in-package :cave-story)

(defun accelerate (pos vel acc)
  "Linear Acceleration. Assuming constant acceleration calc a new (values POS V)."
  (values
   ;; p = p0 + v0t + 1/2 at^2
   (+ pos (* vel frame-time) (* 1/2 acc frame-time frame-time))

   ;; v = v0 + at
   (+ vel (* acc frame-time))))

(defun accelerate-2d (pos vel acc)
  "Vector Acceleration. Assuming constant acceleration calc a new (values POS V)."
  (mvbind (px vx) (accelerate (x pos) (x vel) (x acc))
    (mvbind (py vy) (accelerate (y pos) (y vel) (y acc))
      (values
       (make-v px py)
       (make-v vx vy)))))

(defun friction-accelerate (pos vel friction-acc)
  "Apply friction along the x-axis. Clamps to zero."
  (let ((moving-left? (minusp vel))
	(moving-right? (plusp vel)))
    ;; Apply if we are moving.
    (when (or moving-left? moving-right?)
      ;; Always the opposite direction of motion.
      (let ((acc (if moving-left?
		     friction-acc
		     (- friction-acc))))
	(mvsetq (pos vel) (accelerate pos vel acc))

	;; Has friction overcompensated?
	(when (or (and moving-left? (plusp vel))
		  (and moving-right? (minusp vel)))
	  ;; If so, clamp to zero.
	  (setf vel 0))))
    (values pos vel)))
