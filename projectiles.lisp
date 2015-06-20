(in-package :cave-story)

(defun projectile-collision? (rect dir stage)
  (loop for (tile-pos tile-type) in (stage-get-colliding-tiles stage rect)
     do
       (cond
	 ((wall? tile-type)
	  (return t))
	 ((slope? tile-type)
	  (when (projectile-slope-collision? tile-type tile-pos rect dir)
	    (return t))))))

(defun projectile-slope-collision? (tile-type tile-pos rect dir)
  ;; TODO Merge projectile collisions with player collisions?
  (when (or (and (eq :up dir)    (top-slope? tile-type))
	    (and (eq :down dir)  (bottom-slope? tile-type))
	    (and (eq :left dir)  (left-slope? tile-type))
	    (and (eq :right dir) (right-slope? tile-type)))

    (if (vertical? dir)
	(let* ((x (x (center rect)))
	       (y (tile-slope-pos-y tile-pos tile-type x)))
	  (when (rect-slope-collision? rect x y (opposite-dir dir))
	    t))

	(let* ((y (y (center rect)))
	       (x (tile-slope-pos-x tile-pos tile-type y))
	       (pos (tile-pos->pos tile-pos)))
	  (when (and
		 (<= (x pos) x (+ (x pos) tile-size))
		 (<= (y pos) y (+ (y pos) tile-size))
		 (rect-slope-collision? rect x y (opposite-dir dir)))
	    t)))))

;; Missiles

(defparameter missile-projectile-max-speed 0.3)

(defun missile-projectile-collision-rect (lvl dir pos)
  (let ((short-sides '(10 10 10)))
    (let* ((short-side (elt short-sides lvl))
	   (w (if (vertical? dir) short-side tile-size))
	   (h (if (vertical? dir) tile-size  short-side))
	   (size (make-v w h)))
      (create-rect (+v (tile-dims/2)
		       (-v pos
			   (v/2 size)))
		   size))))

(defun missile-projectile-collisions (rect dir stage)
  (let ((dead? (projectile-collision? rect dir stage)))
    (draw-rect rect yellow)
    dead?))

(defun missile-projectile-pos (m)
  (motion-set-pos (missile-projectile-physics m)))

(def-entity missile-projectile
    (lvl
     dir
     sprite-rect
     dead?)
  (create-missile-projectile (lvl dir pos perp-offset-amt speed acc &optional oscillate?)
			     (let ((perp-dir (if (vertical? dir) :left :up)))
			       (make-missile-projectile
				:lvl lvl
				:dir dir
				:timers
				(alist :life (create-expiring-timer (s->ms 3/2) t))
				:physics
				(let (physics)
				  (asetf physics
					 (alist :kin-2d
						(make-offset-motion
						 (offset-in-dir-pos pos
								    perp-offset-amt
								    perp-dir)
						 dir
						 speed
						 acc)))

				  (when oscillate?
				    (asetf physics
					   (alist :wave-motion
						  (make-wave-motion
						   :dir perp-dir
						   :amp missile-projectile-amplitude
						   :speed missile-radial-speed))))
				  physics)

				:sprite-rect (tile-rect (tile-v (position dir '(:left :up :right :down)) lvl)))))
  :timers :physics :drawable :stage-collision :bullet)

(missile-projectile-method draw (p)
  (draw-sprite :projectile :bullet
	       sprite-rect
	       (missile-projectile-pos p)))

(missile-projectile-methodf stage-collision (p stage)
  (let ((dir (missile-projectile-dir p)))
    (setf dead? (missile-projectile-collisions
		 (missile-projectile-collision-rect lvl dir (missile-projectile-pos p))
		 dir
		 stage))))

(missile-projectile-method bullet-rect (p)
  (missile-projectile-collision-rect lvl dir (missile-projectile-pos p)))

(defmethod bullet-damage-amt ((p missile-projectile))
  3)

(missile-projectile-methodf bullet-hit-react (p)
  (tf dead?))

(missile-projectile-method dead? (p)
  (or (not (timer-active? (aval :life timers)))
      dead?))

(defun make-missile-projectile-group (lvl dir nozzle-pos)
  (let ((pos (sub-v nozzle-pos
		    ;; Subtract the size of the sprite-rect to center it.
		    (tile-dims/2))))
    (if (< lvl 2)
	(list (create-missile-projectile lvl dir pos 0 0.05 0.001))
	(let ((speed 0.0005))
	  (list (create-missile-projectile lvl dir pos 12 speed (rand-val-between 0.0005 0.001) t)
		(create-missile-projectile lvl dir pos 8 speed (rand-val-between 0.001 0.0015) t)
		(create-missile-projectile lvl dir pos 0 speed (rand-val-between 0.001 0.0015) t))))))

(def-entity polar-star-projectile
    (dir
     lvl
     sprite-rect
     dead?)
  (create-polar-star-projectile (nozzle-pos dir lvl)
    (make-polar-star-projectile
     :dir dir
     :lvl lvl
     :timers
     (alist :life (create-expiring-timer (s->ms (elt '(1/8 1/4 1/2) lvl)) t))
     :physics (alist :kin-2d
		     (make-offset-motion
		      (sub-v nozzle-pos (tile-dims/2))
		      dir
		      0.6
		      0))
     :sprite-rect (make-polar-star-projectile-sprite-rect lvl dir)))
  :timers :physics :drawable :bullet :stage-collision)

(polar-star-projectile-methodf ai (p ticks)
  (when (not (timer-active? (aval :life timers)))
    (tf dead?)
    (push-sound :dissipate)
    (make-projectile-star-particle (offset-in-dir-pos (+v (physics-pos p) (tile-dims/2))
						      (tiles/2 1)
						      dir))))

(polar-star-projectile-method physics-pos (p)
  (motion-set-pos physics))

(polar-star-projectile-method draw (p)
  (polar-star-projectile-draw (physics-pos p) sprite-rect))

(polar-star-projectile-method bullet-rect (p)
  (polar-star-projectile-collision-rect lvl dir (physics-pos p)))

(polar-star-projectile-methodf bullet-hit-react (polar-star-projectile)
  (tf dead?))

(polar-star-projectile-method bullet-damage-amt (p)
  (elt '(1 2 4) lvl))

(polar-star-projectile-methodf stage-collision (p stage)
  (let ((pos (physics-pos p)))
    (setf dead? (polar-star-projectile-collisions
		 (polar-star-projectile-collision-rect lvl dir pos)
		 dir
		 pos
		 stage))))

(polar-star-projectile-method dead? (p) dead?)

(defun make-polar-star-projectile-group (lvl dir nozzle-pos)
  (push-sound :polar-star-shoot-3)
  (list (create-polar-star-projectile nozzle-pos dir lvl)))

(defun make-polar-star-projectile-sprite-rect (lvl dir)
  (let ((lvl-tile-positions (mapcar
			     (curry #'apply #'make-v)
			     '((8 2)
			       (10 2)
			       (8 3)))))
    (let ((tp (elt lvl-tile-positions lvl)))
      (when (vertical? dir)
	(incf (x tp)))
      (tile-rect (tile-pos tp)))))

(defun polar-star-projectile-draw (pos sprite-rect)
  (draw-sprite :projectile :bullet
	       sprite-rect
	       pos))

(defun polar-star-projectile-collision-rect (lvl dir pos)
  (let* ((short-side (ecase lvl
		       (0 4)
		       (1 8)
		       (2 16)))
	 (w (if (vertical? dir) short-side tile-size))
	 (h (if (vertical? dir) tile-size  short-side))
	 (size (make-v w h)))
    (create-rect (+v (tile-dims/2)
		     (-v pos
			 (v/2 size)))
		 size)))

(defun polar-star-projectile-collisions (rect dir pos stage)
  (draw-rect rect yellow)
  (let ((dead?))
    (when (projectile-collision? rect dir stage)
      (push-sound :hit-wall)
      (make-projectile-wall-particle
       (offset-in-dir-pos (+v pos (tile-dims/2))
			  (tiles/2 1)
			  dir))
      (tf dead?))
    dead?))

(defun make-projectile-group (gun-name lvl dir nozzle-pos)
  (cons gun-name
	(case gun-name
	  (:polar-star
	   (make-projectile-star-particle nozzle-pos)
	   (make-polar-star-projectile-group lvl dir nozzle-pos))
	  (:missile-launcher
	   (make-missile-projectile-group lvl dir nozzle-pos)))))
