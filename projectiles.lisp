(in-package :cave-story)

(defstruct (missile-projectile (:include entity-state))
  lvl
  dir
  sprite-rect
  dead?)

(defun make-default-missile-projectile 
    (lvl dir pos perp-offset-amt speed acc &optional oscillate?)
  (let ((perp-dir
	 (if (vertical? dir)
	     :left
	     :up)))
    (make-missile-projectile :lvl lvl :dir dir
			     :timers
			     (alist :life
				    (create-expiring-timer
				     (s->ms 3/2) t))
			     :physics
			     (let (physics)
			       (asetf physics
				      (make-offset-motion
				       (offset-in-dir-pos
					pos
					perp-offset-amt
					perp-dir)
				       dir speed acc)
				      :kin-2d)
			       (when oscillate?
				 (asetf physics
					(make-wave-motion
					 :dir perp-dir
					 :amp
					 *missile-projectile-amplitude*
					 :speed
					 *missile-radial-speed*)
					:wave-motion))
			       physics)
			     :sprite-rect
			     (tile-rect
			      (tile-v
			       (position dir
					 '(:left :up
					   :right
					   :down))
			       lvl)))))

(def-entity-constructor create-missile-projectile
    #'make-default-missile-projectile
  :timers :physics :drawable :stage-collision :bullet)

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
		 (<= (x pos) x (+ (x pos) *tile-size*))
		 (<= (y pos) y (+ (y pos) *tile-size*))
		 (rect-slope-collision? rect x y (opposite-dir dir)))
	    t)))))

;; Missiles
(defun missile-projectile-collision-rect (lvl dir pos)
  (let ((short-sides '(10 10 10)))
    (let* ((short-side (elt short-sides lvl))
	   (w (if (vertical? dir) short-side *tile-size*))
	   (h (if (vertical? dir) *tile-size*  short-side))
	   (size (make-v w h)))
      (create-rect (+v (tile-dims/2)
		       (-v pos
			   (v/2 size)))
		   size))))

(defun missile-projectile-collisions (rect dir stage)
  (let ((dead? (projectile-collision? rect dir stage)))
    (draw-rect! rect *yellow*)
    dead?))

(defun missile-projectile-pos (m)
  (motion-set-pos (missile-projectile-physics m)))

(defun missile-projectile-drawing (p)
  (make-sprite-drawing :layer :projectile
		       :sheet-key :bullet
		       :src-rect
		       (missile-projectile-sprite-rect p)
		       :pos (missile-projectile-pos p)))

(defmethod draw ((p missile-projectile))
  (missile-projectile-drawing p))

(defun missile-projectile-stage-collision (p stage)
  (let ((dir (missile-projectile-dir p))
	(lvl (missile-projectile-lvl p)))
    (make-missile-projectile
     :physics (missile-projectile-physics p)
     :timers (missile-projectile-timers p)
     :lvl lvl
     :dir dir
     :sprite-rect (missile-projectile-sprite-rect p)
     :dead?
     (missile-projectile-collisions
      (missile-projectile-collision-rect lvl dir
					 (missile-projectile-pos
					  p))
      dir stage))))

(defmethod stage-collision ((p missile-projectile) stage)
  (missile-projectile-stage-collision p stage))

(defmethod bullet-rect ((p missile-projectile))
  (missile-projectile-collision-rect (missile-projectile-lvl p)
				     (missile-projectile-dir p)
				     (missile-projectile-pos p)))

(defun missile-projectile-damage-amt (p)
  3)

(defmethod bullet-damage-amt ((p missile-projectile))
  (missile-projectile-damage-amt p))

(defun missile-projectile-hit-react (p)
  (make-missile-projectile
   :physics (missile-projectile-physics p)
   :timers (missile-projectile-timers p)
   :lvl (missile-projectile-lvl p)
   :dir (missile-projectile-dir p)
   :sprite-rect (missile-projectile-sprite-rect p)
   :dead? t))

(defmethod bullet-hit-react ((p missile-projectile))
  (missile-projectile-hit-react p))

(defun missile-projectile-ai (p ticks)
  (cond ((member :life ticks)
	 (make-missile-projectile
	  :dead? t))
	(t p)))

(defmethod ai ((p missile-projectile) ticks)
  (missile-projectile-ai p ticks))

(defmethod dead? ((p missile-projectile))
  (missile-projectile-dead? p))

(defun make-missile-projectile-group (lvl dir nozzle-pos)
  (let ((pos (sub-v nozzle-pos
		    ;; Subtract the size of the sprite-rect to center it.
		    (tile-dims/2))))
    (if (< lvl 2)
	(list (create-missile-projectile lvl dir pos 0 0.05 0.001))
	(let ((speed 0.0005))
	  (list (create-missile-projectile lvl dir pos 12 speed
					   (rand-val-between 0.0005 0.001)
					   t)
		(create-missile-projectile lvl dir pos 8 speed
					   (rand-val-between 0.001 0.0015)
					   t)
		(create-missile-projectile lvl dir pos 0 speed
					   (rand-val-between 0.001 0.0015)
					   t))))))

(defstruct (polar-star-projectile (:include entity-state))
  dir
  lvl
  sprite-rect
  dead?)

(defun make-default-polar-star-projectile (nozzle-pos dir lvl)
  (make-polar-star-projectile :dir dir :lvl lvl
			      :timers
			      (alist :life
				     (create-expiring-timer
				      (s->ms
				       (elt
					'(1/8 1/4 1/2)
					lvl))
				      t))
			      :physics
			      (alist :kin-2d
				     (make-offset-motion
				      (sub-v
				       nozzle-pos
				       (tile-dims/2))
				      dir 0.6 0))
			      :sprite-rect
			      (make-polar-star-projectile-sprite-rect
			       lvl dir)))

(def-entity-constructor create-polar-star-projectile
    #'make-default-polar-star-projectile
  :timers :physics :drawable :bullet :stage-collision)

(defun polar-star-projectile-ai (p ticks)
  (cond ((not (timer-active? (aval (polar-star-projectile-timers p) :life)))
	 (push-sound :dissipate)
	 (make-projectile-star-particle
	  (offset-in-dir-pos (+v (physics-pos p) (tile-dims/2))
			     (tiles/2 1)
			     (polar-star-projectile-dir p)))

	 (make-polar-star-projectile
	  :physics (polar-star-projectile-physics p)
	  :timers (polar-star-projectile-timers p)
	  :dir (polar-star-projectile-dir p)
	  :lvl (polar-star-projectile-lvl p)
	  :sprite-rect (polar-star-projectile-sprite-rect p)
	  :dead? t))
	(t p)))

(defmethod ai ((p polar-star-projectile) ticks)
  (polar-star-projectile-ai p ticks))

(defmethod draw ((p polar-star-projectile))
  (polar-star-projectile-drawing (physics-pos p)
				 (polar-star-projectile-sprite-rect p)))

(defmethod bullet-rect ((p polar-star-projectile))
  (polar-star-projectile-collision-rect (polar-star-projectile-lvl p)
					(polar-star-projectile-dir p)
					(physics-pos p)))

(defun polar-star-projectile-hit-react (p)
  (make-polar-star-projectile
   :physics (polar-star-projectile-physics p)
   :timers (polar-star-projectile-timers p)
   :dir (polar-star-projectile-dir p)
   :lvl (polar-star-projectile-lvl p)
   :sprite-rect (polar-star-projectile-sprite-rect p)
   :dead? t))

(defmethod bullet-hit-react ((p polar-star-projectile))
  (polar-star-projectile-hit-react p))

(defun polar-star-projectile-damage-amt (p)
  (elt '(1 2 4) (polar-star-projectile-lvl p)))

(defmethod bullet-damage-amt ((p polar-star-projectile))
  (polar-star-projectile-damage-amt p))

(defun polar-star-projectile-stage-collision (p stage)
  (let ((pos (physics-pos p))
	(lvl (polar-star-projectile-lvl p))
	(dir (polar-star-projectile-dir p)))
    (make-polar-star-projectile
     :physics (polar-star-projectile-physics p)
     :timers (polar-star-projectile-timers p)
     :dir dir
     :lvl lvl
     :sprite-rect (polar-star-projectile-sprite-rect p)
     :dead? (polar-star-projectile-collisions
	     (polar-star-projectile-collision-rect lvl dir pos)
	     dir
	     pos
	     stage))))

(defmethod stage-collision ((p polar-star-projectile) stage)
  (polar-star-projectile-stage-collision p stage))

(defmethod dead? ((p polar-star-projectile))
  (polar-star-projectile-dead? p))

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

(defun polar-star-projectile-drawing (pos sprite-rect)
  (make-sprite-drawing :layer :projectile :sheet-key :bullet
		       :src-rect
		       sprite-rect
		       :pos pos))

(defun polar-star-projectile-collision-rect (lvl dir pos)
  (let* ((short-side (ecase lvl
		       (0 4)
		       (1 8)
		       (2 16)))
	 (w (if (vertical? dir) short-side *tile-size*))
	 (h (if (vertical? dir) *tile-size*  short-side))
	 (size (make-v w h)))
    (create-rect (+v (tile-dims/2)
		     (-v pos
			 (v/2 size)))
		 size)))

(defun polar-star-projectile-collisions (rect dir pos stage)
  (draw-rect! rect *yellow*)
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
