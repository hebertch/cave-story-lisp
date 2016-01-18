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

(defun make-default-camera (focus vel player)
  (amerge
   (camera-fns-alist)
   (alist
    :physics
    (alist
     :target
     (make-target-kin-2d
      :pos focus
      :vel vel
      :target (camera-target-from-player (estate player))
      :target-vel (player-vel (estate player))))
    :player player)))

(def-entity-constructor create-player-camera #'make-default-camera
  :timers :physics)

(defun make-shake ()
  (make-wave-motion :dir :left
		    :amp (tiles 1/8)
		    :speed (rand-val-between 0.017 0.022)))

(defun add-camera-shake ()
  (lambda (physics)
    (aset physics
	  :shake-v (make-shake)
	  :shake-h (make-shake))))

(defun camera-ai (c ticks)
  (let ((physics (aupdate
		  (aval c :physics)
		  (lambda (m)
		    (target-kin-2d-update-target
		     m
		     (camera-target-from-player (estate (aval c :player)))
		     (player-vel (estate (aval c :player)))))
		  :target))
	(shake-tick? (member :shake ticks)))

    (aset c
	  :physics (if shake-tick?
		       (arem physics :shake-h :shake-v)
		       physics)
	  :timers (if shake-tick?
		      (arem (aval c :timers) :shake)
		      (aval c :timers)))))

(defun timed-camera-shake (c time)
  (aset c
	:physics (funcall (add-camera-shake)
			  (aval c :physics))
	:timers (aset (aval c :timers)
		      :shake
		      (create-expiring-timer time t))))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (scale-v *window-dims* 1/2)
	       (sub-v stage-dims *window-dims*)))

(defun camera-focus (c)
  (target-kin-2d-pos (aval (aval c :physics) :target)))

(defun camera-pos (camera camera-bounds)
  (let ((pos (clamp-pos (camera-focus camera) camera-bounds))
	(shake-h (aval (aval camera :physics) :shake-h))
	(shake-v (aval (aval camera :physics) :shake-v)))
    (+v pos
	(if shake-h (wave-offset shake-h) (zero-v))
	(if shake-v (wave-offset shake-v) (zero-v))
	(scale-v *window-dims* -1/2))))
