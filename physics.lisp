(in-package :cave-story)

(defun accelerate (vel acc)
  "Linear Acceleration. Assuming constant acceleration calc a new (values DELTA-POS VEL)."
  (values
   ;; dp = v0t + 1/2 at^2
   (+ (* vel frame-time) (* 1/2 acc frame-time frame-time))

   ;; v = v0 + at
   (+ vel (* acc frame-time))))

(defun accelerate-2d (vel accelerator-x accelerator-y &key clamper-vx clamper-vy)
  "Vector Acceleration. Assuming constant acceleration calc a new (values DELTA-POS VEL)."
  (mvbind (px vx) (funcall accelerator-x (x vel))
    (mvbind (py vy) (funcall accelerator-y (y vel))
      (when clamper-vx
	(setf vx (funcall clamper-vx vx)))
      (when clamper-vy
	(setf vy (funcall clamper-vy vy)))
      (values
       (make-v px py)
       (make-v vx vy)))))

(defmacro physics-2d (pos-form vel-form &rest accelerate-2d-args)
  (with-gensyms (dpos vel)
    `(mvbind (,dpos ,vel) (accelerate-2d ,vel-form ,@accelerate-2d-args)
       (+vf ,pos-form ,dpos)
       (setf ,vel-form ,vel))))

(defun friction-accelerate (vel friction-acc)
  "Apply friction. Clamps to zero."
  (let ((moving-left? (minusp vel))
	(moving-right? (plusp vel))
	(dpos 0))
    ;; Apply if we are moving.
    (when (or moving-left? moving-right?)
      ;; Always the opposite direction of motion.
      (let ((acc (if moving-left?
		     friction-acc
		     (- friction-acc))))
	(mvsetq (dpos vel) (accelerate vel acc))

	;; Has friction overcompensated?
	(when (or (and moving-left? (plusp vel))
		  (and moving-right? (minusp vel)))
	  ;; If so, clamp to zero.
	  (setf vel 0))))

    (values dpos vel)))

(defun const-accelerator (acc)
  (rcurry #'accelerate acc))

(defun friction-accelerator (acc)
  (rcurry #'friction-accelerate acc))

(defun offset-in-dir (dist dir)
  (ecase dir
    (:up    (zero-v :y (- dist)))
    (:down  (zero-v :y dist))
    (:left  (zero-v :x (- dist)))
    (:right (zero-v :x dist))))

(defun offset-in-dir-pos (origin dist dir)
  (+v origin (offset-in-dir dist dir)))

(defstruct offset-motion
  dir
  (dist 0)
  speed

  max-dist
  max-speed
  acc)

(defstruct wave-motion
  dir
  amp
  speed
  (rads (rand-angle)))

(defun offset-motion-physics (om)
  (let (dpos
	(vel (offset-motion-speed om)))
    (aif (offset-motion-acc om)
	 (mvsetq (dpos vel) (accelerate vel it))
	 (setf dpos (accelerate vel 0)))

    (let ((pos (+ dpos (offset-motion-dist om))))
      (awhen (offset-motion-max-dist om)
	(setf pos (clamp-zero pos it)))

      (awhen (offset-motion-max-speed om)
	(setf vel (clamp-zero vel it)))

      (setf (offset-motion-dist om) pos
	    (offset-motion-speed om) vel))))

(defun offset-motion-offset (o)
  (offset-in-dir (offset-motion-dist o)
		 (offset-motion-dir o)))

(defun wave-physics (w)
  (incf (wave-motion-rads w) (* frame-time (wave-motion-speed w))))

(defun wave-offset (w)
  (offset-in-dir (* (wave-motion-amp w) (sin (wave-motion-rads w)))
		 (wave-motion-dir w)))
