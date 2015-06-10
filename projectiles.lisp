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
(defstruct missile-projectile
  sprite-rect
  origin
  offset
  lvl
  dead?
  wave-motion)

(defparameter missile-projectile-max-speed 0.3)
(defun missile-projectile-dir (p)
  (offset-motion-dir (missile-projectile-offset p)))

(defun missile-projectile-physics (p)
  (when (> (offset-motion-dist (missile-projectile-offset p)) (x window-dims))
    (tf (missile-projectile-dead? p)))
  (awhen (missile-projectile-wave-motion p)
    (wave-physics it))
  (offset-motion-physics (missile-projectile-offset p)))

(defun missile-projectile-collision-rect (p)
  (let ((short-sides '(10 10 10)))
    (let* ((short-side (elt short-sides (missile-projectile-lvl p)))
	   (w (if (vertical? (missile-projectile-dir p)) short-side tile-size))
	   (h (if (vertical? (missile-projectile-dir p)) tile-size  short-side))
	   (size (make-v w h)))
      (create-rect (+v (tile-dims/2)
		       (-v (missile-projectile-pos p)
			   (v/2 size)))
		   size))))

(defun missile-projectile-collisions (p stage)
  (let ((rect (missile-projectile-collision-rect p)))
    (draw-rect rect yellow)
    (when (projectile-collision? rect (missile-projectile-dir p) stage)
      (tf (missile-projectile-dead? p)))))

(defun missile-projectile-pos (p)
  (let ((origin (missile-projectile-origin p)))
    (+v (offset-motion-offset (missile-projectile-offset p))
	(aif (missile-projectile-wave-motion p)
	     (wave-offset it)
	     (zero-v))
	origin)))

(defun draw-missile-projectile (mp)
  (draw-sprite :projectile :bullet
	       (missile-projectile-sprite-rect mp)
	       (missile-projectile-pos mp)))

(defun create-missile-projectile (lvl dir pos perp-offset-amt speed acc &optional oscillate?)
  (let* ((sprite-rect (tile-rect (tile-v (position dir '(:left :up :right :down)) lvl)))
	 (p (make-missile-projectile
	     :sprite-rect sprite-rect
	     :origin (offset-in-dir-pos pos perp-offset-amt
					(if (vertical? dir) :left :up))
	     :offset (make-offset-motion
		      :dir dir
		      :speed speed
		      :acc acc)

	     :lvl lvl

	     :wave-motion
	     (if oscillate?
		 (make-wave-motion
		  :dir (if (vertical? dir) :left :up)
		  :amp missile-projectile-amplitude
		  :speed missile-radial-speed)
		 nil)))
	 (dead?-fn (curry #'missile-projectile-dead? p)))
    (def-entity-physics
	(()
	 (missile-projectile-physics p)))
    (def-entity-drawable
	(()
	 (draw-missile-projectile p)))
    (def-entity-stage-collision
	((stage)
	 (missile-projectile-collisions p stage)))

    (def-entity-bullet
	(()
	 (missile-projectile-collision-rect p))
	(()
	 (tf (missile-projectile-dead? p)))
      (()
       -3))
    p))

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

;; Polar Star

(defstruct polar-star-projectile
  sprite-rect
  origin
  offset
  dead?
  lvl)

(defun create-polar-star-projectile (nozzle-pos dir lvl)
  (let* ((p (make-polar-star-projectile
	     :lvl lvl
	     :origin (sub-v nozzle-pos (tile-dims/2))
	     :offset
	     (make-offset-motion :dir dir :speed 0.6)
	     :sprite-rect
	     (make-polar-star-projectile-sprite-rect lvl dir)))
	 (dead?-fn (curry #'polar-star-projectile-dead? p)))
    (def-entity-physics
	(()
	 (polar-star-projectile-physics p)))

    (def-entity-drawable
	(()
	 (polar-star-projectile-draw p)))

    (def-entity-bullet
	(()
	 (polar-star-projectile-collision-rect p))
	(()
	 (tf (polar-star-projectile-dead? p)))
      (()
       -4))
    (def-entity-stage-collision
	((stage)
	 (polar-star-projectile-collisions p stage)))
    p))

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

(defun polar-star-projectile-pos (p)
  (+v (polar-star-projectile-origin p)
      (offset-motion-offset (polar-star-projectile-offset p))))

(defun polar-star-projectile-dir (p)
  (offset-motion-dir (polar-star-projectile-offset p)))

(defun polar-star-projectile-draw (p)
  (draw-sprite :projectile :bullet
	       (polar-star-projectile-sprite-rect p)
	       (polar-star-projectile-pos p)))

(defun polar-star-projectile-physics (p)
  (when (> (offset-motion-dist (polar-star-projectile-offset p))
	   (elt polar-star-projectile-max-offsets (polar-star-projectile-lvl p)))
    (push-sound :dissipate)
    (make-projectile-star-particle (offset-in-dir-pos (+v (polar-star-projectile-pos p) (tile-dims/2))
						      (tiles/2 1)
						      (polar-star-projectile-dir p)))
    (tf (polar-star-projectile-dead? p)))

  (offset-motion-physics (polar-star-projectile-offset p)))

(defun polar-star-projectile-collision-rect (p)
  ;; TODO: Challenge: Don't use gimp to figure out the collision rectangles.
  (let* ((short-side (ecase (polar-star-projectile-lvl p)
		       (0 4)
		       (1 8)
		       (2 16)))
	 (w (if (vertical? (polar-star-projectile-dir p)) short-side tile-size))
	 (h (if (vertical? (polar-star-projectile-dir p)) tile-size  short-side))
	 (size (make-v w h)))
    (create-rect (+v (tile-dims/2)
		     (-v (polar-star-projectile-pos p)
			 (v/2 size)))
		 size)))

(defun polar-star-projectile-collisions (p stage)
  (let ((rect (polar-star-projectile-collision-rect p)))
    (draw-rect rect yellow)
    (when (projectile-collision? rect (polar-star-projectile-dir p) stage)
      (push-sound :hit-wall)
      (make-projectile-wall-particle
       (offset-in-dir-pos (+v (polar-star-projectile-pos p) (tile-dims/2))
			  (tiles/2 1)
			  (polar-star-projectile-dir p)))
      (tf (polar-star-projectile-dead? p)))))

(defun make-projectile-group (gun-name lvl dir nozzle-pos)
  (cons gun-name
	(case gun-name
	  (:polar-star
	   (make-projectile-star-particle nozzle-pos)
	   (cons #'polar-star-projectile-dead?
		 (make-polar-star-projectile-group lvl dir nozzle-pos)))
	  (:missile-launcher
	   (cons #'missile-projectile-dead?
		 (make-missile-projectile-group lvl dir nozzle-pos))))))
