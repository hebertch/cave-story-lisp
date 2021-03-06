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
  (multiple-value-bind (px vx) (call-if accelerator-x (x vel)
					(const-accelerator 0))
    (multiple-value-bind (py vy) (call-if accelerator-y (y vel)
					  (const-accelerator 0))
      (values
       (make-v px py)
       (make-v (call-if clamper-vx vx)
	       (call-if clamper-vy vy))))))

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
  #_(accelerate _ acc))

(defun friction-accelerator (acc)
  #_(friction-accelerate _ acc))

(defun offset-in-dir (dist dir)
  (ecase dir
    (:up    (zero-v :y (- dist)))
    (:down  (zero-v :y dist))
    (:left  (zero-v :x (- dist)))
    (:right (zero-v :x dist))))

(defun offset-in-dir-pos (origin dist dir)
  (+ origin (offset-in-dir dist dir)))

(defun kin-2d-fns-alist ()
  (alist :physics-fn #'kin-2d-motion-physics
	 :pos-fn (lambda (m) (aval m :pos))))

(defun make-kin-2d
    (&key pos vel clamper-vx clamper-vy accelerator-x accelerator-y)
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

(defun motion-set-update (e)
  (loop for key in (aval e :physics) do
       (setq e
	     (aupdate e key #'motion-physics)))
  e)

(defun motion-set-pos (e)
  (let ((pos (zero-v)))
    (loop for key in (aval e :physics) do
	 (setq pos (+ pos (motion-pos (aval e key)))))
    pos))

(defun wave-physics (w)
  (aupdate w :rads #_(+ _ (* *frame-time* (aval w :speed)))))

(defun wave-offset (w)
  (+ (aval w :origin)
     (offset-in-dir (* (aval w :amp) (sin (aval w :rads)))
		    (aval w :dir))))

(defun kin-2d-motion-physics (m)
  (multiple-value-bind (dpos nvel)
      (accelerate-2d (aval m :vel)
		     (aval m :accelerator-x)
		     (aval m :accelerator-y)
		     :clamper-vx (aval m :clamper-vx)
		     :clamper-vy (aval m :clamper-vy))
    (let ((pos
	   (let ((pos (+ (aval m :pos) dpos)))
	     (if (aval m :inertia-vel)
		 (+ pos
		    (accelerate-2d (aval m :inertia-vel)
				   (const-accelerator 0)
				   (const-accelerator 0)))
		 pos))))
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
  (let* ((disp (- (aval m :target)
		  (aval m :pos)))
	 (disp-speeds (abs (* disp (/ *camera-speed-scale-factor*
				      *frame-time*))))
	 (target-speeds (abs (aval m :target-vel)))

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
      (setq m (aupdate m :vel (lambda (vel) (zero-v :y (y vel))))))
    (when (< (abs (y disp)) 1)
      (setq disp (zero-v :x (x disp)))
      (setq m (aupdate m :vel (lambda (vel) (zero-v :x (x vel))))))

    (multiple-value-bind (pos vel)
	(accelerate-2d (aval m :vel)
		       (const-accelerator (* (signum (x disp)) *camera-acc*))
		       (const-accelerator (* (signum (y disp)) *camera-acc*))
		       :clamper-vx clamper-x :clamper-vy clamper-y)
      (aupdate m
	       :pos #_(+ _ pos)
	       :vel (constantly vel)))))
