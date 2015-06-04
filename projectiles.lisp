(in-package :cave-story)

(defgeneric projectile-physics (p))
(defgeneric projectile-collision-rect (p))
(defgeneric projectile-stage-collisions (p stage))
(defgeneric projectile-dead? (p))
(defgeneric projectile-kill (p))

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
  (when (or (and (eq :up dir) (top-slope? tile-type))
	    (and (eq :down dir) (bottom-slope? tile-type))
	    (and (eq :left dir) (left-slope? tile-type))
	    (and (eq :right dir) (right-slope? tile-type)))

    (if (vertical? dir)
	(let* ((x (x (center rect)))
	       (y (tile-slope-pos-y tile-pos tile-type x)))
	  (when (rect-slope-collision? rect (make-v x y) (opposite-dir dir))
	    t))

	(let* ((y (y (center rect)))
	       (x (tile-slope-pos-x tile-pos tile-type y))
	       (pos (tile-pos->pos tile-pos)))
	  (when (and
		 (<= (x pos) x (+ (x pos) tile-size))
		 (<= (y pos) y (+ (y pos) tile-size))
		 (rect-slope-collision? rect (make-v x y) (opposite-dir dir)))
	    t)))))

(defun offset-in-dir-pos (origin offset dir)
  (add-v origin
	 (ecase dir
	   (:up (make-v 0 (- offset)))
	   (:down (make-v 0 offset))
	   (:left (make-v (- offset) 0))
	   (:right (make-v offset 0)))))

(defstruct offset
  origin
  dir
  (dist 0))

(defun offset-physics (o speed acc max-speed)
  "Update offset and return the new speed."
  (mvbind (pos vel) (accelerate (offset-dist o) speed acc)
    (clampf vel (- max-speed) max-speed)
    (setf (offset-dist o) pos)
    vel))

(defun offset-pos (o)
  (offset-in-dir-pos (offset-origin o)
		     (offset-dist o)
		     (offset-dir o)))

;; Missiles
(defstruct missile-projectile
  sprite-rect
  offset
  speed
  (acc 0)
  lvl
  dead?
  rads)

(defparameter missile-projectile-max-speed 0.3)
(defun missile-projectile-dir (p)
  (offset-dir (missile-projectile-offset p)))

(defun missile-projectile-physics (p)
  (when (missile-projectile-rads p)
    (incf (missile-projectile-rads p) missile-radial-speed))
  (setf (missile-projectile-speed p)
	(offset-physics (missile-projectile-offset p)
			(missile-projectile-speed p)
			(missile-projectile-acc p)
			missile-projectile-max-speed)))

(defun missile-projectile-collision-rect (p)
  (let ((short-sides '(10 10 10)))
    (let* ((short-side (elt short-sides (missile-projectile-lvl p)))
	   (w (if (vertical? (missile-projectile-dir p)) short-side tile-size))
	   (h (if (vertical? (missile-projectile-dir p)) tile-size  short-side))
	   (size (make-v w h)))
      (make-rect :pos (add-v (both-v (* tile-size 1/2))
			     (sub-v (missile-projectile-pos p)  (scale-v size 1/2)))
		 :size size))))

(defun missile-projectile-collisions (p stage)
  (let ((rect (missile-projectile-collision-rect p)))
    (push-debug-render (make-rect-drawing
			:color
			#(255 255 0 255)
			:rect rect
			:filled? nil))
    (when (projectile-collision? rect (missile-projectile-dir p) stage)
      (tf (missile-projectile-dead? p)))))

(defun missile-projectile-pos (p)
  (let ((sin-off (if (missile-projectile-rads p)
		     (* missile-projectile-amplitude (sin (missile-projectile-rads p)))
		     0))
	(dir (missile-projectile-dir p)))
    (add-v (offset-pos (missile-projectile-offset p))
	   (offset-in-dir-pos (zero-v)
			      sin-off
			      (if (vertical? dir) :right :down)))))

(defun draw-missile-projectile (mp)
  (push-render
   (make-sprite-drawing :layer :projectile
			:sheet-key :bullet
			:src-rect (missile-projectile-sprite-rect mp)
			:pos (missile-projectile-pos mp))))

(defun rand-angle ()
  (rand-val-between (- pi) pi))

(defun make-missile-projectile-group (lvl dir nozzle-pos)
  (let ((sprite-rect (tile-rect (make-v (position dir '(:left :up :right :down))
					lvl)))
	(pos (sub-v nozzle-pos
		    ;; Subtract the size of the sprite-rect to center it.
		    (both-v (* tile-size 1/2)))))

    (flet ((make-proj (offset speed acc &optional oscillate?)
	     (make-missile-projectile
	      :sprite-rect sprite-rect
	      :offset (make-offset
		       :origin (offset-in-dir-pos pos offset
						  (if (vertical? dir) :left :up))
		       :dir dir)
	      :speed speed
	      :acc acc
	      :lvl lvl
	      :rads (if oscillate? (rand-angle) nil))))

      (if (< lvl 2)
	  (list (make-proj 0 0.05 0.001))
	  (let ((speed 0.0005))
	    (list (make-proj 12 speed (rand-val-between 0.0005 0.001) t)
		  (make-proj 8 speed (rand-val-between 0.001 0.0015) t)
		  (make-proj 0 speed (rand-val-between 0.001 0.0015) t)))))))

;; Polar Star
(defstruct polar-star-projectile
  sprite-rect
  offset
  dead?
  lvl)

(defun make-polar-star-projectile-group (lvl dir nozzle-pos)
  (list (make-polar-star-projectile
	 :lvl lvl
	 :offset
	 (make-offset :origin
		      (sub-v
		       nozzle-pos
		       (both-v (* tile-size 1/2)))
		      :dir dir)
	 :sprite-rect
	 (make-polar-star-projectile-sprite-rect lvl dir))))

(defun make-polar-star-projectile-sprite-rect (lvl dir)
  (let ((lvl-tile-positions (mapcar
			     (curry #'apply #'make-v)
			     '((8 2)
			       (10 2)
			       (8 3)))))
    (let ((tp (elt lvl-tile-positions lvl)))
      (when (vertical? dir)
	(incf (x tp)))
      (tile-rect tp))))

(defun polar-star-projectile-pos (p)
  (offset-pos (polar-star-projectile-offset p)))

(defun polar-star-projectile-dir (p)
  (offset-dir (polar-star-projectile-offset p)))

(defun polar-star-projectile-draw (p)
  (push-render (make-sprite-drawing
		:layer :projectile
		:sheet-key :bullet
		:src-rect
		(polar-star-projectile-sprite-rect p)
		:pos
		(polar-star-projectile-pos p))))

(defun polar-star-projectile-physics (p)
  (when (> (offset-dist (polar-star-projectile-offset p))
	   (elt polar-star-projectile-max-offsets (polar-star-projectile-lvl p)))
    (tf (polar-star-projectile-dead? p)))

  (let ((speed 0.6))
    (offset-physics (polar-star-projectile-offset p) speed 0 speed)))

(defun polar-star-projectile-collision-rect (p)
  ;; TODO: Challenge: Don't use gimp to figure out the collision rectangles.
  (let* ((short-side (ecase (polar-star-projectile-lvl p)
		       (0 4)
		       (1 8)
		       (2 16)))
	 (w (if (vertical? (polar-star-projectile-dir p)) short-side tile-size))
	 (h (if (vertical? (polar-star-projectile-dir p)) tile-size  short-side))
	 (size (make-v w h)))
    (make-rect :pos (add-v (both-v (* tile-size 1/2))
			   (sub-v (polar-star-projectile-pos p)  (scale-v size 1/2)))
	       :size size)))

(defun polar-star-projectile-collisions (p stage)
  (let ((rect (polar-star-projectile-collision-rect p)))
    (push-debug-render (make-rect-drawing
			:color
			#(255 255 0 255)
			:rect rect
			:filled? nil))
    (when (projectile-collision? rect (polar-star-projectile-dir p) stage)
      (tf (polar-star-projectile-dead? p)))))

(defmethod projectile-physics ((p missile-projectile))
  (missile-projectile-physics p))

(defmethod projectile-physics ((p polar-star-projectile))
  (polar-star-projectile-physics p))
(defmethod projectile-stage-collisions ((p missile-projectile) stage)
  (missile-projectile-collisions p stage))
(defmethod projectile-collision-rect ((p missile-projectile))
  (missile-projectile-collision-rect p))

(defmethod projectile-stage-collisions ((p polar-star-projectile) stage)
  (polar-star-projectile-collisions p stage))
(defmethod projectile-collision-rect ((p polar-star-projectile))
  (polar-star-projectile-collision-rect p))
(defmethod projectile-dead? ((p missile-projectile))
  (missile-projectile-dead? p))
(defmethod projectile-kill ((p missile-projectile))
  (tf (missile-projectile-dead? p)))
(defmethod projectile-dead? ((p polar-star-projectile))
  (polar-star-projectile-dead? p))
(defmethod projectile-kill ((p polar-star-projectile))
  (tf (polar-star-projectile-dead? p)))

(defmethod draw ((p missile-projectile))
  (draw-missile-projectile p))

(defmethod draw ((p polar-star-projectile))
  (polar-star-projectile-draw p))
