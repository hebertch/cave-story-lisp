(in-package :cave-story)

(defun missile-projectile-fns-alist ()
  (alist :ai-fn #'missile-projectile-ai
	 :bullet-hit-react-fn #'missile-projectile-hit-react
	 :bullet-damage-amt-fn #'missile-projectile-damage-amt
	 :draw-fn #'missile-projectile-drawing
	 :bullet-rect-fn
	 (lambda (p)
	   (missile-projectile-collision-rect (aval p :lvl)
					      (aval p :dir)
					      (missile-projectile-pos p)))
	 :stage-collision-fn #'missile-projectile-stage-collision))

(defvar* *missile-projectile-subsystems*
  '(:timers :physics :drawable :stage-collision :bullet))

(defun make-missile-projectile 
    (lvl dir pos perp-offset-amt speed acc &optional oscillate?)
  (let ((perp-dir
	 (if (vertical? dir)
	     :left
	     :up)))
    (amerge
     (missile-projectile-fns-alist)
     (alist :subsystems *missile-projectile-subsystems*)
     (when oscillate?
       (alist
	:wave
	(make-wave-motion
	 :dir perp-dir
	 :amp
	 *missile-projectile-amplitude*
	 :speed
	 *missile-radial-speed*)))
     (alist :lvl lvl
	    :dir dir
	    :timers '(:life-timer)
	    :id (gen-entity-id)
	    :life-timer
	    (make-expiring-timer (s->ms 3/2) t)
	    :physics (list* :offset (when oscillate? '(:wave)))
	    :offset
	    (make-offset-motion
	     (offset-in-dir-pos
	      pos
	      perp-offset-amt
	      perp-dir)
	     dir speed acc)
	    :sprite-rect
	    (tile-rect
	     (tile-v
	      (position dir
			'(:left :up
			  :right
			  :down))
	      lvl))))))

(defun missile-projectile-ai (p)
  (aset p :dead? (ticked? p :life-timer)))

(defun projectile-collision? (rect dir stage)
  "Generalized boolean.
Returns the tile of the collision if one occurred."
  (loop for tile in (stage-get-colliding-tiles stage rect)
     do
       (let ((tile-pos (first tile))
	     (tile-type (second tile)))
	 (cond
	   ((wall? tile-type)
	    (return tile))
	   ((slope? tile-type)
	    (when (projectile-slope-collision? tile-type tile-pos rect dir)
	      (return tile)))))))

(defun projectile-slope-collision? (tile-type tile-pos rect dir)
  ;; TODO Merge projectile collisions with player collisions?
  (when (or (and (eq :up dir)    (top-slope? tile-type))
	    (and (eq :down dir)  (bottom-slope? tile-type))
	    (and (eq :left dir)  (left-slope? tile-type))
	    (and (eq :right dir) (right-slope? tile-type)))

    (if (vertical? dir)
	(let* ((x (x (center rect)))
	       (y (tile-slope-pos-y tile-pos tile-type x)))
	  (rect-slope-collision? rect x y (opposite-dir dir)))

	(let* ((y (y (center rect)))
	       (x (tile-slope-pos-x tile-pos tile-type y))
	       (pos (tile-pos->pos tile-pos)))
	  (and
	   (<= (x pos) x (+ (x pos) *tile-size*))
	   (<= (y pos) y (+ (y pos) *tile-size*))
	   (rect-slope-collision? rect x y (opposite-dir dir)))))))

;; Missiles
(defun missile-projectile-collision-rect (lvl dir pos)
  (let ((short-sides '(10 10 10)))
    (let* ((short-side (elt short-sides lvl))
	   (w (if (vertical? dir) short-side *tile-size*))
	   (h (if (vertical? dir) *tile-size*  short-side))
	   (size (make-v w h)))
      (create-rect (+ (tile-dims/2)
		      (- pos
			 (v/2 size)))
		   size))))

(defun missile-projectile-collisions (rect dir stage)
  (draw-rect! rect *yellow*)
  (projectile-collision? rect dir stage))

(defun missile-projectile-pos (m)
  (physics-pos m))

(defun missile-projectile-drawing (p)
  (list (make-sprite-drawing :layer :projectile
			     :sheet-key :bullet
			     :src-rect
			     (aval p :sprite-rect)
			     :pos (missile-projectile-pos p))))


(defun missile-projectile-stage-collision (p stage)
  (let ((dir (aval p :dir))
	(lvl (aval p :lvl)))
    (let ((tile
	   (missile-projectile-collisions
	    (missile-projectile-collision-rect lvl dir
					       (missile-projectile-pos
						p))
	    dir stage)))
      (projectile-tile-collision p (asetfn :dead? t) stage tile))))


(defun missile-projectile-damage-amt (p)
  (declare (ignore p))
  3)

(defun missile-projectile-hit-react (p)
  (aset p :dead? t))

(defun add-missile-projectile-group (obj lvl dir nozzle-pos)
  (let* ((pos (- nozzle-pos
		 ;; Subtract the size of the sprite-rect to center it.
		 (tile-dims/2)))
	 (pg
	  (if (< lvl 2)
	      (list (make-missile-projectile lvl dir pos 0 0.05 0.001))
	      (let ((speed 0.0005))
		(list (make-missile-projectile lvl dir pos 12 speed
					       (rand-val-between 0.0005 0.001)
					       t)
		      (make-missile-projectile lvl dir pos 8 speed
					       (rand-val-between 0.001 0.0015)
					       t)
		      (make-missile-projectile lvl dir pos 0 speed
					       (rand-val-between 0.001 0.0015)
					       t))))))
    (aupdate obj
	     :new-states
	     (appendfn (list*
			(projectile-groups-add
			 (estate (aval *global-game*
				       :projectile-groups))
			 (cons :missile-launcher
			       (mapcar (avalfn :id) pg)))
			pg)))))

(defun polar-star-projectile-fns-alist ()
  (alist :ai-fn #'polar-star-projectile-ai
	 :bullet-hit-react-fn #'polar-star-projectile-hit-react
	 :bullet-damage-amt-fn #'polar-star-projectile-damage-amt
	 :draw-fn #'polar-star-projectile-drawing
	 :bullet-rect-fn
	 (lambda (p)
	   (polar-star-projectile-collision-rect (aval p :lvl)
						 (aval p :dir)
						 (physics-pos p)))
	 :stage-collision-fn #'polar-star-projectile-stage-collision))

(defvar* *polar-star-projectile-subsystems*
  '(:timers :physics :drawable :bullet :stage-collision))

(defun make-polar-star-projectile (nozzle-pos dir lvl)
  (amerge
   (polar-star-projectile-fns-alist)
   (alist :subsystems *polar-star-projectile-subsystems*)
   (alist :dir dir :lvl lvl
	  :timers '(:life-timer)
	  :id (gen-entity-id)
	  :life-timer
	  (make-expiring-timer (s->ms (elt '(1/8 1/4 1/2) lvl)) t)
	  :physics '(:offset)
	  :offset
	  (make-offset-motion
	   (- nozzle-pos (tile-dims/2))
	   dir 0.6 0)
	  :sprite-rect
	  (make-polar-star-projectile-sprite-rect lvl dir))))

(defun polar-star-projectile-ai (p)
  (cond ((not (timer-active? (aval p :life-timer)))
	 (aupdate p
		  :dead? (constantly t)
		  :sound-effects (pushfn :snd-polar-star-l1-2)
		  :new-states
		  (pushfn (make-projectile-star-particle
			   (offset-in-dir-pos (+ (physics-pos p) (tile-dims/2))
					      (tiles/2 1)
					      (aval p :dir))))))
	(t p)))

(defun polar-star-projectile-hit-react (p)
  (aset p :dead? t))

(defun polar-star-projectile-damage-amt (p)
  (elt '(1 2 4) (aval p :lvl)))

(defun projectile-tile-collision (p projectile-reaction stage tile)
  (if tile
      (aupdate (funcall projectile-reaction p)
	       :new-states
	       (pushfn (stage-tile-shot stage tile)))
      p))

(defun polar-star-projectile-stage-collision (p stage)
  (let ((pos (physics-pos p))
	(lvl (aval p :lvl))
	(dir (aval p :dir)))
    (let ((tile
	   (polar-star-projectile-collisions
	    (polar-star-projectile-collision-rect lvl dir pos)
	    dir stage))
	  (react (aupdatefn
		  :dead? (constantly t)
		  :new-states
		  (pushfn (make-projectile-wall-particle
			   (offset-in-dir-pos (+ pos (tile-dims/2))
					      (tiles/2 1)
					      dir)))
		  :sound-effects (pushfn :snd-shot-hit))))
      (projectile-tile-collision p react stage tile))))

(defun add-polar-star-projectile-group (obj lvl dir nozzle-pos)
  (let ((pg (list (make-polar-star-projectile nozzle-pos dir lvl))))
    (aupdate obj
	     :sound-effects (pushfn :snd-polar-star-l3)
	     :new-states
	     (appendfn
	      (list (make-projectile-star-particle nozzle-pos)
		    (projectile-groups-add
		     (estate (aval *global-game* :projectile-groups))
		     (cons :polar-star (mapcar (avalfn :id) pg))))
	      pg))))

(defun make-polar-star-projectile-sprite-rect (lvl dir)
  (let ((lvl-tile-positions (mapcar
			     (curry #'apply #'make-v)
			     '((8 2)
			       (10 2)
			       (8 3)))))
    (let ((tp (elt lvl-tile-positions lvl)))
      (tile-rect
       (tile-pos (if (vertical? dir)
		     (make-v (1+ (x tp)) (y tp))
		     tp))))))

(defun polar-star-projectile-drawing (p)
  (list (make-sprite-drawing :layer :projectile
			     :sheet-key :bullet
			     :src-rect (aval p :sprite-rect)
			     :pos (physics-pos p))))

(defun polar-star-projectile-collision-rect (lvl dir pos)
  (let* ((short-side (ecase lvl
		       (0 4)
		       (1 8)
		       (2 16)))
	 (w (if (vertical? dir) short-side *tile-size*))
	 (h (if (vertical? dir) *tile-size*  short-side))
	 (size (make-v w h)))
    (create-rect (+ (tile-dims/2)
		    (- pos
		       (v/2 size)))
		 size)))

(defun polar-star-projectile-collisions (rect dir stage)
  (draw-rect! rect *yellow*)
  (projectile-collision? rect dir stage))

(defun add-projectile-group (obj gun-name lvl dir nozzle-pos)
  (case gun-name
    (:polar-star
     (add-polar-star-projectile-group obj lvl dir nozzle-pos))
    (:missile-launcher
     (add-missile-projectile-group obj lvl dir nozzle-pos))))
