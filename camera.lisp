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

(def-entity camera
    (player)
  (create-player-camera (focus vel player)
			(make-camera :physics (alist :target
						     (make-target-kin-2d :pos focus :vel vel
									 :target (camera-target-from-player (estate player))
									 :target-vel (player-vel (estate player))))
				     :player player))
  :ai :physics)

(camera-methodf ai (c ticks)
  (aupdatef
   physics
   (lambda (m)
     (target-kin-2d-update-target
      m
      (camera-target-from-player (estate player))
      (player-vel (estate player))))
   :keys '(:target)))

(defun camera-focus (c)
  (target-kin-2d-pos (aval :target (camera-physics c))))

(defparameter camera-speed-scale-factor 1/20)
(defparameter camera-acc 2e-4)
(defparameter camera-max-speed 0.15859374)

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v window-dims 1/2)
	       (sub-v stage-dims window-dims)))

(defun camera-focus->camera-pos (cf)
  (sub-v cf (scale-v window-dims 1/2)))
