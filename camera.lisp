(in-package :cave-story)

(defvar camera)

(defstructure camera
  focus
  vel
  target-fn
  target-vel-fn)

(defun camera-target-from-player (player)
  (let ((h-facing (player-h-facing player))
	(v-facing (player-v-facing player))
	(pos (+v (player-pos player)
		 (tile-dims/2))))

    (+v
     (if v-facing
	 (offset-in-dir (tiles/2 7) v-facing)
	 (zero-v))
     (offset-in-dir-pos pos (tiles 3) h-facing))))

(defun create-player-camera (focus vel player)
  (let ((c (make-camera :focus focus :vel vel
			:target-vel-fn
			(lambda ()
			  (player-vel player))
			:target-fn
			(lambda ()
			  (camera-target-from-player player))))
	(dead?-fn))
    (def-entity-physics (() (camera-physics c)))
    c))

(defparameter camera-speed-scale-factor 1/20)
(defparameter camera-acc 2e-4)
(defparameter camera-max-speed 0.15859374)

(defun camera-physics (camera)
  (let* ((target-vel (funcall (camera-target-vel-fn camera)))
	 (camera-target (funcall (camera-target-fn camera)))
	 (camera-focus (camera-focus camera))
	 (camera-vel (camera-vel camera))
	 (disp (sub-v camera-target camera-focus))
	 (disp-speeds (abs-v (scale-v disp (/ camera-speed-scale-factor frame-time))))
	 (target-speeds (abs-v target-vel))

	 ;; Camera velocity clamped by speed proportional to distance, and by a max speed
	 (clamper-x
	  (clamper-zero
	   (* (signum (x disp))
	      (min (x disp-speeds)
		   (+ (x target-speeds) camera-max-speed)))))
	 (clamper-y
	  (clamper-zero
	   (* (signum (y disp))
	      (min (y disp-speeds)
		   (+ (y target-speeds) camera-max-speed))))))

    ;; When disp is less than 1 pixel distance, don't accelerate.
    ;; This is to avoid shaking.

    (when (< (abs (x disp)) 1)
      (allf 0 (x disp) (x camera-vel)))
    (when (< (abs (y disp)) 1)
      (allf 0 (y disp) (y camera-vel)))

    (physics-2d
     (camera-focus camera) (camera-vel camera)
     (const-accelerator (* (signum (x disp)) camera-acc))
     (const-accelerator (* (signum (y disp)) camera-acc))
     :clamper-vx clamper-x
     :clamper-vy clamper-y)

    (draw-line camera-focus
	       (+v camera-focus
		   (*v (camera-vel camera) debug-velocity-scale))
	       cyan)))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v window-dims 1/2)
	       (sub-v stage-dims window-dims)))

(defun camera-focus->camera-pos (cf)
  (sub-v cf (scale-v window-dims 1/2)))
