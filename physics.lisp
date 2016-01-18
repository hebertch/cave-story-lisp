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
  (unless accelerator-x (setq accelerator-x (const-accelerator 0)))
  (unless accelerator-y (setq accelerator-y (const-accelerator 0)))
  (multiple-value-bind (px vx) (funcall accelerator-x (x vel))
    (multiple-value-bind (py vy) (funcall accelerator-y (y vel))
      (values
       (make-v px py)
       (make-v (if clamper-vx
		   (funcall clamper-vx vx)
		   vx)
	       (if clamper-vy
		   (funcall clamper-vy vy)
		   vy))))))

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
	(multiple-value-setq (dpos vel) (accelerate vel acc))

	;; Has friction overcompensated?
	(when (or (and moving-left? (plusp vel))
		  (and moving-right? (minusp vel)))
	  ;; If so, clamp to zero.
	  (setq vel 0))))

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

(defun kin-2d-fns-alist ()
  (alist :physics-fn #'kin-2d-motion-physics
	 :pos-fn (lambda (m) (aval m :pos))))

(defun make-kin-2d (&key pos vel clamper-vx clamper-vy accelerator-x accelerator-y)
  (amerge
   (kin-2d-fns-alist)
   (alist
    :pos pos
    :vel vel
    :clamper-vx clamper-vx
    :clamper-vy clamper-vy
    :accelerator-x accelerator-x
    :accelerator-y accelerator-y)))

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

(defun wave-motion-fns-alist ()
  (alist :physics-fn #'wave-physics
	 :pos-fn #'wave-offset))

(defun make-wave-motion (&key (origin (zero-v)) dir amp speed (rads (rand-angle)))
  (amerge
   (wave-motion-fns-alist)
   (alist :origin origin
	  :dir dir
	  :amp amp
	  :speed speed
	  :rads rads)))

(defun motion-set-update (physics)
  (loop for asc in physics
     collecting
       (cons (car asc) (motion-physics (cdr asc)))))

(defun motion-set-pos (physics)
  (let ((pos (zero-v)))
    (loop for (k . m) in physics do
	 (setq pos (+v pos (motion-pos m))))
    pos))

(defun wave-physics (w)
  (aset w :rads (+ (aval w :rads)
		   (* *frame-time* (aval w :speed)))))

(defun wave-offset (w)
  (+v (aval w :origin)
      (offset-in-dir (* (aval w :amp) (sin (aval w :rads)))
		     (aval w :dir))))

(defun kin-2d-motion-physics (m)
  (multiple-value-bind (dpos nvel)
      (accelerate-2d (aval m :vel)
		     (aval m :accelerator-x)
		     (aval m :accelerator-y)
		     :clamper-vx (aval m :clamper-vx)
		     :clamper-vy (aval m :clamper-vy))
    (let ((pos (+v (aval m :pos) dpos)))
      (when (aval m :inertia-vel)
	(setq pos
	      (+v pos
		  (accelerate-2d (aval m :inertia-vel)
				 (const-accelerator 0)
				 (const-accelerator 0)))))
      (aset m
	    :pos pos
	    :vel nvel))))

(defun target-kin-2d-fns-alist ()
  (alist :physics-fn #'target-kin-2d-motion-physics
	 :pos-fn (lambda (m) (aval m :pos))))

(defun make-target-kin-2d (pos vel target target-vel)
  (amerge
   (target-kin-2d-fns-alist)
   (alist
    :pos pos
    :vel vel
    :target target
    :target-vel target-vel)))

(defun target-kin-2d-update-target (m targ targ-vel)
  (aset m
	:target targ
	:target-vel targ-vel))

(defun target-kin-2d-motion-physics (m)
  (let* ((disp (sub-v (aval m :target)
		      (aval m :pos)))
	 (disp-speeds (abs-v (scale-v disp (/ *camera-speed-scale-factor*
					      *frame-time*))))
	 (target-speeds (abs-v (aval m :target-vel)))

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

    (when (< (abs (x disp)) 1)
      (setq disp (zero-v :y (y disp)))
      (setq m (aset m :vel (zero-v :y (y (aval m :vel))))))
    (when (< (abs (y disp)) 1)
      (setq disp (zero-v :x (x disp)))
      (setq m (aset m :vel (zero-v :x (x (aval m :vel))))))

    (multiple-value-bind (pos vel)
	(accelerate-2d (aval m :vel)
		       (const-accelerator (* (signum (x disp)) *camera-acc*))
		       (const-accelerator (* (signum (y disp)) *camera-acc*))
		       :clamper-vx clamper-x :clamper-vy clamper-y)
      (aset m
	    :pos  (+v (aval m :pos) pos)
	    :vel vel))))