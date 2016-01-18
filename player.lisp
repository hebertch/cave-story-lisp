(in-package :cave-story)

(defun player-fns-alist ()
  (alist :stage-collision-fn #'player-stage-collision
	 :ai-fn #'player-ai
	 :origin-fn #'physics-tile-origin
	 :input-fn #'player-input
	 :draw-fn #'player-and-gun-drawing))

(defun make-default-player (hud projectile-groups damage-numbers gun-exps
			    active-systems)
  (let ((id (gen-entity-id)))
    (values
     (amerge
      (player-fns-alist)
      (alist :h-facing :left
	     :gun-name-cycle (create-cycle *gun-names*)
	     :health-amt 3
	     :max-health-amt 3
	     :damage-numbers damage-numbers
	     :gun-exps gun-exps
	     :projectile-groups projectile-groups
	     :active-systems active-systems
	     :hud hud
	     :id id
	     :timers
	     (alist :walk-cycle
		    (create-timed-cycle 12
					#(0 1 0
					  2)
					t)
		    :invincible
		    (create-expiring-timer
		     (s->ms 3)))
	     :physics
	     (list
	      (cons :stage
		    (alist
		     :pos (scale-v *window-dims* 1/2)
		     :vel (zero-v)
		     :clamper-vx
		     (clamper+- *player-max-speed-x*)
		     :clamper-vy
		     (clamper+- *terminal-speed*))))))
     id)))

(def-entity-constructor create-default-player #'make-default-player
  :timers :input :physics :stage-collision :drawable)

(defparameter *player-walk-acc* 0.00083007812)
(defparameter *player-max-speed-x* 0.15859375)
(defparameter *player-friction-acc* 0.00049804687)
(defparameter *terminal-speed* 0.2998046875)
(defparameter *gravity-acc* 0.00078125)
(defparameter *player-jump-gravity-acc* 0.0003125)
(defparameter *player-air-acc* 0.0003125)
(defparameter *player-jump-speed* 0.25)
(defparameter *player-hop-speed* (/ *player-jump-speed* 1.5))

(defparameter *player-collision-rectangles-alist*
  (loop for (key x y w h) in
       '((:bottom 11 16 10 15)
	 (:top 7 2 18 15)
	 (:left 6 8 10 12)
	 (:right 16 8 10 12))
     collect (cons key (make-rect :pos (make-v x y) :size (make-v w h)))))

(defun player-collision-rect (side)
  (cdr (assoc side *player-collision-rectangles-alist*)))

(defparameter *player-damage-rect*
  (let ((left (left (player-collision-rect :left)))
	(right (right (player-collision-rect :right)))
	(top (top (player-collision-rect :top)))
	(bottom (bottom (player-collision-rect :bottom))))
    (make-rect :pos (make-v left top)
	       :size (make-v (- right left)
			     (- bottom top)))))

(defun player-pickup! (p pickup)
  (ecase (pickup-type pickup)
    (:dorito
     (player-gun-exp! p (pickup-amt pickup))))
  (values))

(defun player-state (p)
  (estate p))

(defun player-walk-acc (p)
  (const-accelerator
   (* (if (player-on-ground? p)
	  *player-walk-acc*
	  *player-air-acc*)
      (case (aval p :acc-dir)
	(:left -1)
	(:right 1)
	(t 0)))))

(defun player-friction-acc ()
  (friction-accelerator *player-friction-acc*))

(defun player-accelerator-x (p)
  (cond
    ((and (player-on-ground? p)
	  (null (aval p :acc-dir)))
     (player-friction-acc))
    (t (player-walk-acc p))))

(defun player-accelerator-y (p kin-2d)
  (const-accelerator
   (if (and (minusp (y (aval kin-2d :vel)))
	    (aval p :jumping?))
       *player-jump-gravity-acc*
       *gravity-acc*)))

(defun player-inertia-vel (p ground-inertia-entity-state)
  (if (eq (aval p :ground-tile) :dynamic)
      (inertia-vel ground-inertia-entity-state)
      nil))

(defun player-kin-2d-physics (p kin-2d)
  (draw-line! (aval kin-2d :pos)
	      (+v (aval kin-2d :pos)
		  (*v (aval kin-2d :vel) *debug-velocity-scale*))
	      *magenta*)

  (aset kin-2d
	:accelerator-x (player-accelerator-x p)
	:accelerator-y (player-accelerator-y p kin-2d)
	:inertia-vel
	(player-inertia-vel p (estate (aval p :ground-inertia-entity)))))

(defun apply-player-physics (p)
  (aupdate (aval p :physics)
	   (lambda (kin-2d)
	     (player-kin-2d-physics p kin-2d))
	   :stage))

(defun player-ai (p ticks)
  (when (and (find :walk-cycle ticks)
	     (/= 0 (timed-cycle-current (aval (aval p :timers) :walk-cycle))))
    (push-sound :step))
  (aset p :physics (apply-player-physics p)))

(defun player-fire-gun! (p)
  (let ((gun-name (player-current-gun-name p)))
    (let ((num-projectile-groups
	   (projectile-groups-count (estate (aval p :projectile-groups))
				    gun-name))
	  (nozzle-pos (player-nozzle-pos p))
	  (dir (if (player-actual-v-facing p)
		   (player-actual-v-facing p)
		   (aval p :h-facing)))
	  (lvl (gun-level (gun-exp-for (estate (aval p :gun-exps)) gun-name)
			  (cdr (assoc gun-name *gun-level-exps*))))
	  (max-projectiles (cdr (assoc gun-name *max-projectile-groups*))))
      (unless (null max-projectiles)
	(when (< num-projectile-groups max-projectiles)
	  (replace-entity-state
	   (aval p :projectile-groups)
	   (rcurry #'projectile-groups-add
		   (make-projectile-group gun-name lvl dir nozzle-pos)))))))
  (values))

(defun player-short-hop-physics (kin-2d)
  (aset kin-2d
	:vel
	(make-v (x (aval kin-2d :vel))
		(min (y (aval kin-2d :vel))
		     (- *player-hop-speed*)))))

(defun player-take-damage (p dmg-amt)
  (if (not (timer-active? (aval (aval p :timers) :invincible)))
      (cond
	((>= (abs dmg-amt) (aval p :health-amt))
	 (push-sound :player-die)
	 (stop-music)
	 (estate-set (aval p :active-systems)
		     (active-systems-switch-to-dialog))

	 (aset p
	       :health-amt 0
	       :dead? t))
	(t
	 (push-sound :hurt)
	 (replace-entity-state (aval p :hud) #'hud-health-changed)
	 (replace-entity-state
	  (aval p :gun-exps)
	  (lambda (g)
	    (incr-gun-exp
	     g
	     (player-current-gun-name p)
	     (- (* 2 dmg-amt)))))

	 (update-damage-number-amt (aval p :damage-numbers)
				   (aval p :id)
				   dmg-amt)
	 (aset p
	       :timers (aupdate (aval p :timers) #'reset-timer :invincible)
	       :ground-tile nil
	       :health-amt (- (aval p :health-amt) dmg-amt)
	       :physics
	       (aupdate (aval p :physics)
			#'player-short-hop-physics
			:stage))))
      p))

(defun player-gun-exp! (p amt)
  (replace-entity-state
   (aval p :gun-exps)
   (lambda (g)
     (incr-gun-exp g (player-current-gun-name p) amt)))
  (replace-entity-state (aval p :hud) #'hud-exp-changed)
  (values))

(defun char-sprite-pos (h-facing v-facing interacting? walk-idx)
  "Grabs the tile-pos for the character given the character's state."
  (let ((y (ecase h-facing
	     (:left 0)
	     (:right 1))))
    (if interacting?
	(tile-v 7 y)
	(let ((x (case v-facing
		   (:up (+ 3 walk-idx))
		   (:down 6)
		   (t
		    (assert (<= 0 walk-idx 2))
		    walk-idx))))
	  (tile-v x y)))))

(defun player-on-ground? (p)
  (aval p :ground-tile))

(defun player-actual-v-facing (p)
  "The player cannot actually face down when on the ground."
  (let ((v-facing (aval p :v-facing))
	(on-ground? (player-on-ground? p)))
    (if (and on-ground? (eq v-facing :down))
	nil
	v-facing)))

(defun player-walk-idx (player)
  (timed-cycle-current (aval (aval player :timers) :walk-cycle)))

(defun player-walking? (p)
  (and (aval p :acc-dir) (player-on-ground? p)))

(defun player-sprite-rect
    (h-facing actual-v-facing interacting? walking? walk-idx vel-y)
  "The sprite-rect for player P."
  (tile-rect (char-sprite-pos
	      h-facing
	      actual-v-facing
	      interacting?
	      (cond
		(walking? walk-idx)
		((plusp vel-y) 1)
		((minusp vel-y) 2)
		(t 0)))))

(defun player-jump-physics (kin-2d)
  (aset kin-2d
	:vel
	(+v (aval kin-2d :vel)
	    (make-v 0 (- *player-jump-speed*)))))

(defun player-jump (p)
  (cond ((not (aval p :jumping?))
	 (amerge
	  (when (player-on-ground? p)
	    (push-sound :jump)

	    (alist :physics
		   (aupdate (aval p :physics)
			    #'player-jump-physics
			    :stage)
		   :interacting? nil
		   :ground-tile nil
		   :timers (aupdate (aval p :timers) #'timed-cycle-pause :walk-cycle)))

	  (alist :jumping? t)
	  p))
	(t p)))

(defun player-move (p dir)
  "Moves the player in a horizontal direction."
  (let ((timers (aval p :timers)))
    (when (player-on-ground? p)
      (setq timers (aupdate timers
			    #'timed-cycle-resume
			    :walk-cycle))
      (when (null (aval p :acc-dir))
	(setq timers (aupdate timers
			      #'timed-cycle-restart
			      :walk-cycle))))
    (aset p
	  :timers timers
	  :interacting? nil
	  :acc-dir dir
	  :h-facing dir)))

(defun player-input (p input)
  (let ((left?  (or (key-held? input :left)
		    (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right)
		    (eq :positive (input-joy-axis-x input))))
	(down?  (or (key-held? input :down)
		    (eq :positive (input-joy-axis-y input))))
	(up?    (or (key-held? input :up)
		    (eq :negative (input-joy-axis-y input)))))

    (let* ((on-ground? (player-on-ground? p))
	   (walking? (player-walking? p)))
      (cond
	;; Look Up/Down or Interact
	((and up? (not down?))
	 (setq p (aset p
		       :interacting? nil
		       :v-facing :up)))

	((and down? (not up?))
	 (unless (eq (aval p :v-facing) :down)
	   (setq p (aset p
			 :v-facing :down
			 :timers
			 (aupdate (aval p :timers)
				  #'timed-cycle-pause
				  :walk-cycle)
			 :interacting? on-ground?))))

	(t (setq p (aset p :v-facing nil))))

      (cond
	;; Walk/Look based on horizontal direction
	((and left? (not right?))
	 (setq p (player-move p :left)))

	((and right? (not left?))
	 (setq p (player-move p :right)))

	(t
	 (when walking?
	   (setq p (aset p
			 :timers
			 (aupdate (aval p :timers)
				  #'timed-cycle-pause
				  :walk-cycle)))
	   (push-sound :step))
	 (setq p (aset p :acc-dir nil))))

      (if (or (key-held? input :z) (joy-held? input :a))
	  (setq p (player-jump p))
	  (setq p (aset p :jumping? nil))))

    (when (or (key-pressed? input :a) (joy-pressed? input :y))
      (setq p (aset :gun-name-cycle (cycle-previous (aval p :gun-name-cycle)))))

    (when (or (key-pressed? input :s) (joy-pressed? input :x))
      (setq p (aset :gun-name-cycle (cycle-next (aval p :gun-name-cycle)))))
    p))


(defun player-stage-collision (p stage)
  (let* ((collision-rects *player-collision-rectangles-alist*)
	 (physics (aval p :physics))
	 (stage-physics (aval physics :stage))
	 (data (alist :pos (aval stage-physics :pos)
		      :vel (aval stage-physics :vel)
		      :ground-tile (aval p :ground-tile)
		      :new-ground-tile nil))
	 (res (stage-collisions
	       data stage collision-rects
	       (let ((stop-x
		      (collision-lambda (data)
			(aset data :vel (zero-v :y (y (aval data :vel)))))))
		 (alist :bottom
			(collision-lambda (data)
			  (unless (aval data :ground-tile)
			    (push-sound :land))

			  (aset
			   data
			   :vel (zero-v :x (x (aval data :vel)))
			   :new-ground-tile (aval data :tile-type)))
			:left stop-x :right stop-x
			:top
			(collision-lambda (tile-type)
			  (when (minusp (y (aval data :vel)))
			    (push-sound :head-bump))
			  (aset data
				:vel
				(make-v
				 (x (aval data :vel))
				 (max (y (aval data :vel)) 0))))))
	       (aval p :ground-tile))))
    (amerge
     (alist
      :physics
      (aset (aval p :physics)
	    :stage
	    (aset stage-physics
		  :pos (aval res :pos)
		  :vel (aval res :vel)))
      :ground-tile (aval res :new-ground-tile))
     (unless (aval p :ground-tile)
       (alist :timers
	      (aupdate (aval p :timers) #'timed-cycle-pause :walk-cycle)
	      :ground-tile nil))
     p)))

(defun player-drawing (p)
  (let ((kin-2d (cdr (assoc :stage (aval p :physics)))))
    (make-sprite-drawing :layer :player
			 :sheet-key :my-char
			 :src-rect (player-sprite-rect (aval p :h-facing)
						       (player-actual-v-facing p)
						       (aval p :interacting?)
						       (player-walking? p)
						       (player-walk-idx p)
						       (y (aval kin-2d :vel)))
			 :pos (pixel-v (aval kin-2d :pos)))))

(defun player-and-gun-drawing (p)
  (let ((kin-2d (cdr (assoc :stage (aval p :physics)))))
    (unless
	(and (timer-active? (aval (aval p :timers) :invincible))
	     (plusp (chunk-timer-period
		     (aval (aval p :timers) :invincible) 50)))
      (list
       (player-drawing p)
       (player-gun-drawing (aval kin-2d :pos)
			   (player-current-gun-name p)
			   (aval p :h-facing)
			   (player-actual-v-facing p)
			   (player-walking? p)
			   (player-walk-idx p))))))

(defun player-vel (p)
  (aval (aval (aval p :physics) :stage) :vel))

(defun player-damage-collision-rect (p)
  (rect-offset *player-damage-rect* (physics-pos p)))

(defun player-current-gun-name (p)
  (cycle-current (aval p :gun-name-cycle)))
