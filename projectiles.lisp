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

(defmethod ai ((p missile-projectile))
  (modify-missile-projectile (p)
    (when (> (offset-motion-dist offset) (x window-dims))
      (tf dead?))))

(defmethod physics ((p missile-projectile))
  (modify-missile-projectile (p)
    (when wave-motion
      (fnf wave-motion #'wave-physics))
    (fnf offset #'offset-motion-physics)))

(defmethod draw ((p missile-projectile))
  (with-missile-projectile-slots (p)
    (draw-sprite :projectile :bullet
		 sprite-rect
		 (missile-projectile-pos origin offset wave-motion))))

(defmethod stage-collision ((p missile-projectile) stage)
  (modify-missile-projectile (p)
    (setf dead? (missile-projectile-collisions
		 (missile-projectile-collision-rect lvl dir (missile-projectile-pos origin offset wave-motion))
		 dir
		 stage))))

(defmethod bullet-rect ((p missile-projectile))
  (with-missile-projectile-slots (p)
    (missile-projectile-collision-rect lvl dir (missile-projectile-pos origin offset wave-motion))))

(defmethod bullet-damage-amt ((p missile-projectile))
  3)

(defmethod bullet-hit-react ((p missile-projectile))
  (modify-missile-projectile (p)
    (tf dead?)))

(defmethod dead? ((p missile-projectile))
  (missile-projectile-dead? p))

(defun create-missile-projectile (lvl dir pos perp-offset-amt speed acc &optional oscillate? (id (gen-entity-id)))
  (let ((perp-dir (if (vertical? dir) :left :up)))
    (create-entity
     (make-missile-projectile :lvl lvl
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
			      :sprite-rect (tile-rect (tile-v (position dir '(:left :up :right :down)) lvl)))
     '(:ai :physics :drawable :stage-collision :bullet)
     :id id)))

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

(defmethod ai ((p polar-star-projectile))
  (modify-polar-star-projectile (p)
    (setf dead? (polar-star-projectile-ai offset lvl (polar-star-projectile-pos origin offset) dir))))

(defmethod physics ((p polar-star-projectile))
  (modify-polar-star-projectile (p)
    (fnf offset #'polar-star-projectile-physics)))

(defmethod draw ((p polar-star-projectile))
  (with-polar-star-projectile-slots (p)
    (polar-star-projectile-draw (polar-star-projectile-pos origin offset) sprite-rect)))

(defmethod bullet-rect ((p polar-star-projectile))
  (with-polar-star-projectile-slots (p)
    (polar-star-projectile-collision-rect lvl dir (polar-star-projectile-pos origin offset))))

(defmethod bullet-hit-react ((p polar-star-projectile))
  (modify-polar-star-projectile (p)
    (tf dead?)))

(defmethod bullet-damage-amt ((p polar-star-projectile))
  (elt '(1 2 4) (polar-star-projectile-lvl p)))

(defmethod stage-collision ((p polar-star-projectile) stage)
  (modify-polar-star-projectile (p)
    (let ((pos (polar-star-projectile-pos origin offset)))
      (setf dead? (polar-star-projectile-collisions
		   (polar-star-projectile-collision-rect lvl dir pos)
		   dir
		   pos
		   stage)))))

(defmethod dead? ((p polar-star-projectile))
  (polar-star-projectile-dead? p))

;; Polar Star
(defun create-polar-star-projectile (nozzle-pos dir lvl &key (id (gen-entity-id)))
  (create-entity
   (make-polar-star-projectile :nozzle-pos nozzle-pos
			       :dir dir
			       :lvl lvl)
   '(:ai :physics :drawable :bullet :stage-collision)
   :id id))

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
