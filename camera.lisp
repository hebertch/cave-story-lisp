(in-package :cave-story)

(defun camera-target-from-player (player)
  (let ((h-facing (player-h-facing player))
	(v-facing (player-v-facing player))
	(pos (+v (physics-pos player)
		 (tile-dims/2))))

    (+v
     (if v-facing
	 (offset-in-dir (tiles/2 7) v-facing)
	 (zero-v))
     (offset-in-dir-pos pos (tiles 3) h-facing))))

(def-entity camera (player)
  (create-player-camera
   (focus vel player)
   (make-camera :physics (alist :target
				(make-target-kin-2d :pos focus :vel vel
						    :target (camera-target-from-player (estate player))
						    :target-vel (player-vel (estate player))))
		:player player))
  :timers :physics)

(defun make-shake ()
  (make-wave-motion :dir :left :amp (tiles 1/8) :speed (rand-val-between 0.017 0.022)))

(defun add-camera-shake ()
  (compose
   (asetfn (make-shake) :shake-h)
   (asetfn (make-shake) :shake-v)))

(camera-methodf ai (c ticks)
  (when (member :shake ticks)
    (aremf physics :shake-h :shake-v)
    (aremf timers :shake))
  (aupdatef
   physics
   (lambda (m)
     (target-kin-2d-update-target
      m
      (camera-target-from-player (estate player))
      (player-vel (estate player))))
   '(:target)))

(defun timed-camera-shake (c time)
  (modify-camera (c)
    (fnf physics (add-camera-shake))
    (asetf timers (create-expiring-timer time t) :shake)))

(defun camera-focus (c)
  (target-kin-2d-pos (aval (camera-physics c) :target)))

(defparameter camera-speed-scale-factor 1/20)
(defparameter camera-acc 2e-4)
(defparameter camera-max-speed 0.15859374)

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v window-dims 1/2)
	       (sub-v stage-dims window-dims)))

(defun camera-pos (camera camera-bounds)
  (let ((pos (clamp-pos (camera-focus camera) camera-bounds)))
    (awhen (aval (camera-physics camera) :shake-h)
      (+vf pos (wave-offset it)))
    (awhen (aval (camera-physics camera) :shake-v)
      (+vf pos (wave-offset it)))
    (+vf pos (scale-v window-dims -1/2))))
