(in-package :cave-story)

(defparameter *camera-speed-scale-factor* 1/20)
(defparameter *camera-acc* 2e-4)
(defparameter *camera-max-speed* 0.15859374)

(defun camera-target-from-player (player)
  (let ((h-facing (aval player :h-facing))
	(v-facing (aval player :v-facing))
	(pos (+v (physics-pos player)
		 (tile-dims/2))))

    (+v
     (if v-facing
	 (offset-in-dir (tiles/2 7) v-facing)
	 (zero-v))
     (offset-in-dir-pos pos (tiles 3) h-facing))))

(defun camera-fns-alist ()
  (alist :ai-fn #'camera-ai))

(defparameter *camera-subsystems* '(:timers :physics))

(defun make-camera (focus vel player)
  (amerge
   (camera-fns-alist)
   (alist :subsystems *camera-subsystems*)
   (alist
    :target
    (make-target-kin-2d
     focus vel
     (camera-target-from-player (estate player))
     (player-vel (estate player)))
    :player player)))

(defun make-shake ()
  (make-wave-motion :dir :left
		    :amp (tiles 1/8)
		    :speed (rand-val-between 0.017 0.022)))

(defun add-camera-shake ()
  (lambda (physics)
    (aset physics
	  :shake-v (make-shake)
	  :shake-h (make-shake))))

(defun camera-ai (c)
  (let ((shake-tick? (member :shake (aval c :ticks))))

    (aset c
	  :target
	  (target-kin-2d-update-target
	   (aval c :target)
	   (camera-target-from-player (estate (aval c :player)))
	   (player-vel (estate (aval c :player))))
	  :physics (if shake-tick?
		       (arem (aval c :physics) :shake-h :shake-v)
		       (aval c :physics))
	  :timers (if shake-tick?
		      (arem (aval c :timers) :shake)
		      (aval c :timers)))))

(defun timed-camera-shake (c time)
  (aset c
	:physics (funcall (add-camera-shake)
			  (aval c :physics))
	:timers (aset (aval c :timers)
		      :shake
		      (make-expiring-timer time t))))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v *window-dims* 1/2)
	       (sub-v stage-dims *window-dims*)))

(defun camera-focus (c)
  (aval (aval c :target) :pos))

(defun camera-pos (camera camera-bounds)
  (let ((pos (clamp-pos (camera-focus camera) camera-bounds))
	(shake-h (aval (aval camera :physics) :shake-h))
	(shake-v (aval (aval camera :physics) :shake-v)))
    (+v pos
	(if shake-h (wave-offset shake-h) (zero-v))
	(if shake-v (wave-offset shake-v) (zero-v))
	(scale-v *window-dims* -1/2))))
