(in-package :cave-story)

(defvar* *camera-speed-scale-factor* 1/20)
(defvar* *camera-acc* 2e-4)
(defvar* *camera-max-speed* 0.15859374)

(defun camera-target-from-player (player)
  (let ((h-facing (aval player :h-facing))
	(v-facing (aval player :v-facing))
	(pos (+ (physics-pos player)
		(tile-dims/2))))

    (+
     (if v-facing
	 (offset-in-dir (tiles/2 7) v-facing)
	 (zero-v))
     (offset-in-dir-pos pos (tiles 3) h-facing))))

(defun camera-fns-alist ()
  (alist :ai-fn #'camera-ai))

(defvar* *camera-subsystems* '(:timers :physics))

(defun make-camera (focus vel player)
  (amerge
   (camera-fns-alist)
   (alist :subsystems *camera-subsystems*)
   (alist
    :id (gen-entity-id)
    :target
    (make-target-kin-2d
     focus vel
     (camera-target-from-player player)
     (player-vel player))
    :physics '(:target))))

(defun make-shake ()
  (make-wave-motion :dir :left
		    :amp (tiles 1/8)
		    :speed (rand-val-between 0.017 0.022)))

(setfn add-camera-shake
       (aupdatefn
	:physics
	(adjoinfn :shake-v :shake-h))
       (asetfn
	:shake-v (make-shake)
	:shake-h (make-shake)))

(defun camera-ai (c)
  (let ((shake-tick? (ticked? c :shake-timer)))
    (aupdate c
	     :target
	     (constantly (let ((player (estate (entity-id :player))))
			   (if player
			       (target-kin-2d-update-target
				(aval c :target)
				(camera-target-from-player player)
				(player-vel player))
			       (aval c :target))))
	     :physics (when shake-tick?
			(removefn :shake-v :shake-h))
	     :timers (when shake-tick?
		       (removefn :shake-timer))
	     :shake-timer (when shake-tick?
			    (constantly nil)))))

(defun timed-camera-shake (c time)
  (funcall
   (comp
    (aupdatefn
     :shake-timer (constantly (make-expiring-timer time t))
     :timers (adjoinfn :shake-timer))
    add-camera-shake)
   c))

(defun stage-dims->camera-bounds (stage-dims)
  (create-rect (* *window-dims* 1/2)
	       (- stage-dims *window-dims*)))

(defun camera-focus (c)
  (aval (aval c :target) :pos))

(defun camera-pos (camera camera-bounds)
  (let ((pos (clamp-pos (camera-focus camera) camera-bounds))
	(shake-h (aval camera :shake-h))
	(shake-v (aval camera :shake-v)))
    (+ pos
       (if shake-h (wave-offset shake-h) (zero-v))
       (if shake-v (wave-offset shake-v) (zero-v))
       (* *window-dims* -1/2))))