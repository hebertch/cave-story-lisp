(in-package :cave-story)

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

(defstructure camera
    focus
  vel
  player)

(defmethod physics ((c camera))
  (modify-camera (c)
    (let ((player (player-state player)))
      (mvsetq (focus vel)
	      (camera-physics (camera-target-from-player player)
			      (player-vel player)
			      focus
			      vel)))))

(defun create-player-camera (focus vel player &key (id (gen-entity-id)))
  (comment-code
    (register-entity-interface
     id
     (dlambda
      (:focus () (with-camera-slots (c) focus)))))
  (create-entity
   (make-camera :focus focus :vel vel :player player)
   '(:physics)
   :id id))

(defparameter camera-speed-scale-factor 1/20)
(defparameter camera-acc 2e-4)
(defparameter camera-max-speed 0.15859374)

(defun camera-physics (target target-vel focus vel)
  (let* ((disp (sub-v target focus))
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

    (setf vel (copy-v2 vel))
    (when (< (abs (x disp)) 1)
      (allf 0 (x disp) (x vel)))
    (when (< (abs (y disp)) 1)
      (allf 0 (y disp) (y vel)))

    (physics-2d
     focus vel
     (const-accelerator (* (signum (x disp)) camera-acc))
     (const-accelerator (* (signum (y disp)) camera-acc))
     :clamper-vx clamper-x
     :clamper-vy clamper-y)

    (draw-line focus
	       (+v focus
		   (*v vel debug-velocity-scale))
	       cyan)
    (values focus vel)))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v window-dims 1/2)
	       (sub-v stage-dims window-dims)))

(defun camera-focus->camera-pos (cf)
  (sub-v cf (scale-v window-dims 1/2)))
