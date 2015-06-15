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

(defun missile-projectile-pos (origin offset wave-motion)
  (+v (offset-motion-offset offset)
      (aif wave-motion
	   (wave-offset it)
	   (zero-v))
      origin))

(defstructure missile-projectile
    lvl
  dir
  origin
  offset
  wave-motion
  sprite-rect
  dead?)

(defun create-missile-projectile (lvl dir pos perp-offset-amt speed acc &optional oscillate? (id (gen-entity-id)))
  (let* ((perp-dir (if (vertical? dir) :left :up))
	 (p (make-missile-projectile :lvl lvl
				     :dir dir
				     :origin (offset-in-dir-pos pos
								perp-offset-amt
								perp-dir)
				     :offset (make-offset-motion :dir dir :speed speed :acc acc)
				     :wave-motion (if oscillate?
						      (make-wave-motion
						       :dir perp-dir
						       :amp missile-projectile-amplitude
						       :speed missile-radial-speed)
						      nil)
				     :sprite-rect (tile-rect (tile-v (position dir '(:left :up :right :down)) lvl))))
	 (dead?-fn (lambda () (missile-projectile-dead? p))))

    (def-entity-ai
	(()
	 (modify-missile-projectile (p)
	   (when (> (offset-motion-dist offset) (x window-dims))
	     (tf dead?)))))

    (def-entity-physics
	(()
	 (modify-missile-projectile (p)
	   (when wave-motion
	     (fnf wave-motion #'wave-physics))
	   (fnf offset #'offset-motion-physics))))

    (def-entity-drawable
	(()
	 (with-missile-projectile-slots (p)
	   (draw-sprite :projectile :bullet
			sprite-rect
			(missile-projectile-pos origin offset wave-motion)))))

    (def-entity-stage-collision
	((stage)
	 (modify-missile-projectile (p)
	   (setf dead? (missile-projectile-collisions
			(missile-projectile-collision-rect lvl dir (missile-projectile-pos origin offset wave-motion))
			dir
			stage)))))

    (def-entity-bullet
	(()
	 (with-missile-projectile-slots (p)
	   (missile-projectile-collision-rect lvl dir (missile-projectile-pos origin offset wave-motion))))
	(()
	 (modify-missile-projectile (p)
	   (tf dead?)))
      (()
       3))
    (register-entity-interface
     id
     (dlambda
      (:dead? () (missile-projectile-dead? p))))))

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

(defstructure polar-star-projectile
    nozzle-pos
  dir
  lvl
  (offset (make-offset-motion :dir dir :speed 0.6))
  dead?
  (origin (sub-v nozzle-pos (tile-dims/2)))
  (sprite-rect (make-polar-star-projectile-sprite-rect lvl dir)))

;; Polar Star
(defun create-polar-star-projectile (nozzle-pos dir lvl &key (id (gen-entity-id)))
  (let* ((p (make-polar-star-projectile :nozzle-pos nozzle-pos
					:dir dir
					:lvl lvl))
	 (dead?-fn (lambda () (polar-star-projectile-dead? p))))

    (def-entity-ai
	(()
	 (setf p (with-polar-star-projectile-copy-slots (p)
		   (setf dead? (polar-star-projectile-ai offset lvl (polar-star-projectile-pos origin offset) dir))
		   p))))

    (def-entity-physics
	(()
	 (setf p (with-polar-star-projectile-copy-slots (p)
		   (fnf offset #'polar-star-projectile-physics)
		   p))))

    (def-entity-drawable
	(()
	 (with-polar-star-projectile-slots (p)
	   (polar-star-projectile-draw (polar-star-projectile-pos origin offset) sprite-rect))))

    (def-entity-bullet
	(()
	 (with-polar-star-projectile-slots (p)
	   (polar-star-projectile-collision-rect lvl dir (polar-star-projectile-pos origin offset))))
	(()
	 (setf p (with-polar-star-projectile-copy-slots (p)
		   (tf dead?)
		   p)))
      (()
       (elt '(1 2 4) lvl)))

    (def-entity-stage-collision
	((stage)
	 (setf p (with-polar-star-projectile-copy-slots (p)
		   (let ((pos (polar-star-projectile-pos origin offset)))
		     (setf dead? (polar-star-projectile-collisions
				  (polar-star-projectile-collision-rect lvl dir pos)
				  dir
				  pos
				  stage)))
		   p))))

    (register-entity-interface
     id
     (dlambda
      (:dead? () (polar-star-projectile-dead? p))))))

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

(defun polar-star-projectile-pos (origin offset)
  (+v origin (offset-motion-offset offset)))

(defun polar-star-projectile-draw (pos sprite-rect)
  (draw-sprite :projectile :bullet
	       sprite-rect
	       pos))

(defun polar-star-projectile-ai (offset lvl pos dir)
  (let ((dead? (> (offset-motion-dist offset)
		  (elt polar-star-projectile-max-offsets lvl))))
    (when dead?
      (push-sound :dissipate)
      (make-projectile-star-particle (offset-in-dir-pos (+v pos (tile-dims/2))
							(tiles/2 1)
							dir)))
    dead?))

(defun polar-star-projectile-physics (offset)
  (offset-motion-physics offset))

(defun polar-star-projectile-collision-rect (lvl dir pos)
  ;; TODO: Challenge: Don't use gimp to figure out the collision rectangles.
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
