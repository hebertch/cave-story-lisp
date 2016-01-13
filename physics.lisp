(in-package :cave-story)

(defun accelerate (vel acc)
  "Linear Acceleration. Assuming constant acceleration calc a
new (values DELTA-POS VEL)."
  (values
   ;; dp = v0t + 1/2 at^2
   (+ (* vel *frame-time*) (* 1/2 acc *frame-time* *frame-time*))

   ;; v = v0 + at
   (+ vel (* acc *frame-time*))))

(defun accelerate-2d (vel accelerator-x accelerator-y &key clamper-vx clamper-vy)
  "Vector Acceleration. Assuming constant acceleration calc a
new (values DELTA-POS VEL)."
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

(defun make-offset-motion (origin dir speed &optional (acc 0))
  (make-kin-2d
   :pos origin
   :vel (offset-in-dir-pos (zero-v) speed dir)
   :accelerator-x (const-accelerator (case dir
				       (:left (- acc))
				       (:right acc)
				       (t 0)))
   :accelerator-y (const-accelerator (case dir
				       (:up (- acc))
				       (:down acc)
				       (t 0)))))

(defstruct wave-motion
  (origin (zero-v))
  dir
  amp
  speed
  (rads (rand-angle)))

(defun motion-set-update (physics)
  (aupdate physics #'motion-physics))
(defmacro motion-set-updatef (physics)
  `(fnf ,physics #'motion-set-update))

(defun motion-set-pos (physics)
  (let ((pos (zero-v)))
    (doalist (k m physics)
      (+vf pos (motion-pos m)))
    pos))

(defun wave-physics (w)
  (let ((w (copy-structure w)))
    (incf (wave-motion-rads w) (* *frame-time* (wave-motion-speed w)))
    w))

(defun wave-offset (w)
  (+v (wave-motion-origin w)
      (offset-in-dir (* (wave-motion-amp w) (sin (wave-motion-rads w)))
		     (wave-motion-dir w))))

(defmethod motion-physics ((w wave-motion))
  (wave-physics w))
(defmethod motion-pos ((w wave-motion))
  (wave-offset w))

(defstruct kin-2d
  (pos (zero-v))
  (vel (zero-v))
  (accelerator-x (const-accelerator 0))
  (accelerator-y (const-accelerator 0))
  clamper-vx
  clamper-vy

  inertia-vel)


(defmethod motion-physics ((m kin-2d))
  (mvbind (dpos nvel)
      (accelerate-2d (kin-2d-vel m)
		     (kin-2d-accelerator-x m)
		     (kin-2d-accelerator-y m)
		     :clamper-vx (kin-2d-clamper-vx m)
		     :clamper-vy (kin-2d-clamper-vy m))
    (+vf (kin-2d-pos m) dpos)
    (awhen (kin-2d-inertia-vel m)
      (+vf (kin-2d-pos m)
	   (accelerate-2d it (const-accelerator 0) (const-accelerator 0))))
    (setf (kin-2d-vel m) nvel))
  m)

(defmethod motion-pos ((m kin-2d))
  (kin-2d-pos m))

(defstruct target-kin-2d
  pos
  vel
  target
  target-vel)

(defun target-kin-2d-update-target (m targ targ-vel)
  (setf (target-kin-2d-target m) targ
	(target-kin-2d-target-vel m) targ-vel)
  m)

(defmethod motion-physics ((m target-kin-2d))
  (let* ((disp (sub-v (target-kin-2d-target m)
		      (target-kin-2d-pos m)))
	 (disp-speeds (abs-v (scale-v disp (/ *camera-speed-scale-factor*
					      *frame-time*))))
	 (target-speeds (abs-v (target-kin-2d-target-vel m)))

	 ;; Camera velocity clamped by speed proportional to distance,
	 ;; and by a max speed
	 (clamper-x
	  (clamper-zero
	   (* (signum (x disp))
	      (min (x disp-speeds)
		   (+ (x target-speeds) *camera-max-speed*)))))
	 (clamper-y
	  (clamper-zero
	   (* (signum (y disp))
	      (min (y disp-speeds)
		   (+ (y target-speeds) *camera-max-speed*))))))

    ;; When disp is less than 1 pixel distance, don't accelerate.
    ;; This is to avoid shaking.

    (setf (target-kin-2d-vel m) (copy-v2 (target-kin-2d-vel m)))
    (when (< (abs (x disp)) 1)
      (allf 0 (x disp) (x (target-kin-2d-vel m))))
    (when (< (abs (y disp)) 1)
      (allf 0 (y disp) (y (target-kin-2d-vel m))))

    (physics-2d
     (target-kin-2d-pos m) (target-kin-2d-vel m)
     (const-accelerator (* (signum (x disp)) *camera-acc*))
     (const-accelerator (* (signum (y disp)) *camera-acc*))
     :clamper-vx clamper-x
     :clamper-vy clamper-y))
  m)

(defmethod motion-pos ((m target-kin-2d))
  (target-kin-2d-pos m))
