(in-package :cave-story)

(defparameter *camera-speed-scale-factor* 1/20)
(defparameter *camera-acc* 2e-4)
(defparameter *camera-max-speed* 0.15859374)

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

(defstruct (camera (:include entity-state)) player)

(defun make-default-camera (focus vel player)
  (make-camera :physics
	       (alist :target
		      (make-target-kin-2d :pos focus
					  :vel vel
					  :target
					  (camera-target-from-player
					   (estate
					    player))
					  :target-vel
					  (player-vel
					   (estate
					    player))))
	       :player player))
(def-entity-constructor create-player-camera #'make-default-camera
  :timers :physics)

(defun make-shake ()
  (make-wave-motion :dir :left :amp (tiles 1/8) :speed (rand-val-between 0.017 0.022)))

(defun add-camera-shake ()
  (compose
   (asetfn (make-shake) :shake-h)
   (asetfn (make-shake) :shake-v)))

(defmethod ai ((c camera) ticks)
  (let ((physics (aupdate
		  (camera-physics c)
		  (lambda (m)
		    (target-kin-2d-update-target
		     m
		     (camera-target-from-player (estate (camera-player c)))
		     (player-vel (estate (camera-player c)))))
		  '(:target)))
	(shake-tick? (member :shake ticks)))

    (make-camera
     :physics (if shake-tick?
		  (arem physics :shake-h :shake-v)
		  physics)
     :timers (if shake-tick?
		 (arem (camera-timers c) :shake)
		 (camera-timers c))
     :player (camera-player c))))

(defun timed-camera-shake (c time)
  (make-camera :physics (funcall (add-camera-shake)
				 (camera-physics c))
	       :timers (aset (camera-timers c)
			     (create-expiring-timer time t)
			     :shake)
	       :player (camera-player c)))

(defun camera-focus (c)
  (target-kin-2d-pos (aval (camera-physics c) :target)))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v *window-dims* 1/2)
	       (sub-v stage-dims *window-dims*)))

(defun camera-pos (camera camera-bounds)
  (let ((pos (clamp-pos (camera-focus camera) camera-bounds)))
    (awhen (aval (camera-physics camera) :shake-h)
      (+vf pos (wave-offset it)))
    (awhen (aval (camera-physics camera) :shake-v)
      (+vf pos (wave-offset it)))
    (+vf pos (scale-v *window-dims* -1/2))))
