(in-package :cave-story)

(defstruct (player (:include entity-state))
  dead?
  
  h-facing
  v-facing
  interacting?
  ground-tile
  jumping?
  gun-name-cycle
  health-amt
  max-health-amt
  id
  hud
  active-systems
  ground-inertia-entity
  damage-numbers
  gun-exps
  projectile-groups
  acc-dir)

(defun make-default-player (hud projectile-groups damage-numbers gun-exps
			    active-systems)
  (let ((id (gen-entity-id)))
    (values
     (make-player :h-facing :left
		  :gun-name-cycle
		  (create-cycle *gun-names*)
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
			 (make-kin-2d :pos
				      (scale-v
				       *window-dims*
				       1/2)
				      :vel (zero-v)
				      :clamper-vx
				      (clamper+-
				       *player-max-speed-x*)
				      :clamper-vy
				      (clamper+-
				       *terminal-speed*)))))
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
      (case (player-acc-dir p)
	(:left -1)
	(:right 1)
	(t 0)))))

(defun player-friction-acc ()
  (friction-accelerator *player-friction-acc*))

(defun player-accelerator-x (p)
  (cond
    ((and (player-on-ground? p)
	  (null (player-acc-dir p)))
     (player-friction-acc))
    (t (player-walk-acc p))))

(defun player-accelerator-y (p kin-2d)
  (const-accelerator
   (if (and (minusp (y (kin-2d-vel kin-2d)))
	    (player-jumping? p))
       *player-jump-gravity-acc*
       *gravity-acc*)))

(defun player-inertia-vel (p ground-inertia-entity-state)
  (if (eq (player-ground-tile p) :dynamic)
      (inertia-vel ground-inertia-entity-state)
      nil))

(defun player-kin-2d-physics (p kin-2d)
  (draw-line! (kin-2d-pos kin-2d)
	      (+v (kin-2d-pos kin-2d)
		  (*v (kin-2d-vel kin-2d) *debug-velocity-scale*))
	      *magenta*)

  (make-kin-2d :pos (kin-2d-pos kin-2d)
	       :vel (kin-2d-vel kin-2d)
	       :accelerator-x
	       (player-accelerator-x p)
	       :accelerator-y
	       (player-accelerator-y p kin-2d)
	       :clamper-vx (kin-2d-clamper-vx kin-2d)
	       :clamper-vy (kin-2d-clamper-vy kin-2d)
	       :inertia-vel
	       (player-inertia-vel p (estate (player-ground-inertia-entity p)))))

(defun apply-player-physics (p)
  (aupdate (player-physics p)
	   (lambda (kin-2d)
	     (player-kin-2d-physics p kin-2d))
	   '(:stage)))

(defun player-ai (p ticks)
  (when (and (find :walk-cycle ticks)
	     (/= 0 (timed-cycle-current (aval (player-timers p) :walk-cycle))))
    (push-sound :step))

  (make-player
   :physics (apply-player-physics p)
   :timers (player-timers p)
   :h-facing (player-h-facing p)
   :v-facing (player-v-facing p)
   :interacting? (player-interacting? p)
   :ground-tile (player-ground-tile p)
   :jumping? (player-jumping? p)
   :gun-name-cycle (player-gun-name-cycle p)
   :health-amt (player-health-amt p)
   :max-health-amt (player-max-health-amt p)
   :id (player-id p)
   :hud (player-hud p)
   :active-systems (player-active-systems p)
   :ground-inertia-entity (player-ground-inertia-entity p)
   :damage-numbers (player-damage-numbers p)
   :gun-exps (player-gun-exps p)
   :projectile-groups (player-projectile-groups p)
   :acc-dir (player-acc-dir p)))

(defmethod ai ((p player) ticks)
  (player-ai p ticks))

(defmethod dead? ((p player))
  (player-dead? p))

(defun player-fire-gun! (p)
  (let ((gun-name (player-current-gun-name p)))
    (let ((num-projectile-groups
	   (projectile-groups-count (estate (player-projectile-groups p))
				    gun-name))
	  (nozzle-pos (player-nozzle-pos p))
	  (dir (aif (player-actual-v-facing p)
		    it
		    (player-h-facing p)))
	  (lvl (gun-level (gun-exp-for (estate (player-gun-exps p)) gun-name)
			  (cdr (assoc gun-name *gun-level-exps*))))
	  (max-projectiles (cdr (assoc gun-name *max-projectile-groups*))))
      (unless (null max-projectiles)
	(when (< num-projectile-groups max-projectiles)
	  (replace-entity-state
	   (player-projectile-groups p)
	   (rcurry #'projectile-groups-add
		   (make-projectile-group gun-name lvl dir nozzle-pos)))))))
  (values))

(defun player-take-damage (p dmg-amt)
  (let ((p (copy-player p)))
    (unless (timer-active? (aval (player-timers p) :invincible))
      (cond
	((>= (abs dmg-amt) (player-health-amt p))
	 (push-sound :player-die)
	 (stop-music)
	 (replace-entity-state (player-active-systems p)
			       #'active-systems-switch-to-dialog)

	 (setf (player-health-amt p) 0)
	 (setf (player-dead? p) t)
	 (let ((*entity-system-type* :dialog))
	   (comment-code
	     (create-callback-timer
	      (s->ms 1/2)
	      (create-game-over-event)))))
	(t
	 (aupdatef (player-timers p) #'reset-timer '(:invincible))
	 (nilf (player-ground-tile p))
	 (push-sound :hurt)
	 (replace-entity-state (player-hud p) #'hud-health-changed)
	 (decf (player-health-amt p) dmg-amt)
	 (replace-entity-state
	  (player-gun-exps p)
	  (lambda (g)
	    (incr-gun-exp
	     g
	     (player-current-gun-name p)
	     (- (* 2 dmg-amt)))))
	 (update-damage-number-amt (player-damage-numbers p)
				   (player-id p)
				   dmg-amt)
	 (aupdatef
	  (player-physics p)
	  (lambda (kin-2d)
	    (setf (kin-2d-vel kin-2d)
		  (make-v (x (kin-2d-vel kin-2d))
			  (min (y (kin-2d-vel kin-2d)) (- *player-hop-speed*))))
	    kin-2d)
	  '(:stage)))))
    p))

(defmethod origin ((p player))
  (physics-tile-origin p))

(defun player-gun-exp! (p amt)
  (replace-entity-state
   (player-gun-exps p)
   (lambda (g)
     (incr-gun-exp g (player-current-gun-name p) amt)))
  (replace-entity-state (player-hud p) #'hud-exp-changed)
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
  (player-ground-tile p))

(defun player-actual-v-facing (p)
  "The player cannot actually face down when on the ground."
  (let ((v-facing (player-v-facing p))
	(on-ground? (player-on-ground? p)))
    (if (and on-ground? (eq v-facing :down))
	nil
	v-facing)))

(defun player-walk-idx (player)
  (timed-cycle-current (aval (player-timers player) :walk-cycle)))

(defun player-walking? (p)
  (and (player-acc-dir p) (player-on-ground? p)))

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

(defun player-jump (p)
  (let ((p (copy-structure p)))
    (unless (player-jumping? p)
      (when (player-on-ground? p)
	(aupdatef
	 (player-physics p)
	 (lambda (kin-2d)
	   (setf (kin-2d-vel kin-2d)
		 (+v (kin-2d-vel kin-2d) (make-v 0 (- *player-jump-speed*))))
	   kin-2d)
	 '(:stage))

	(push-sound :jump))
      (nilf (player-interacting? p) (player-ground-tile p))
      (aupdatef (player-timers p) #'timed-cycle-pause '(:walk-cycle))
      (tf (player-jumping? p)))
    p))

(defun player-move (p dir)
  "Moves the player in a horizontal direction."
  (let ((p (copy-structure p)))
    (when (player-on-ground? p)
      (aupdatef (player-timers p) #'timed-cycle-resume '(:walk-cycle))
      (when (null (player-acc-dir p))
	(aupdatef (player-timers p) #'timed-cycle-restart '(:walk-cycle))))

    (nilf (player-interacting? p))
    (allf dir (player-acc-dir p) (player-h-facing p))
    p))

(defun player-input (p input)
  (let ((left?  (or (key-held? input :left)
		    (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right)
		    (eq :positive (input-joy-axis-x input))))
	(down?  (or (key-held? input :down)
		    (eq :positive (input-joy-axis-y input))))
	(up?    (or (key-held? input :up)
		    (eq :negative (input-joy-axis-y input))))
	(p (copy-player p)))

    (let* ((on-ground? (player-on-ground? p))
	   (walking? (player-walking? p)))
      (cond
	;; Look Up/Down or Interact
	((and up? (not down?))
	 (nilf (player-interacting? p))
	 (setf (player-v-facing p) :up))

	((and down? (not up?))
	 (unless (eq (player-v-facing p) :down)
	   (setf (player-v-facing p) :down)
	   (aupdatef (player-timers p) #'timed-cycle-pause '(:walk-cycle))
	   (setf (player-interacting? p) on-ground?)))

	(t (nilf (player-v-facing p))))

      (cond
	;; Walk/Look based on horizontal direction
	((and left? (not right?))
	 (fnf p (rcurry #'player-move :left)))

	((and right? (not left?))
	 (fnf p (rcurry #'player-move :right)))

	(t
	 (when walking?
	   (aupdatef (player-timers p) #'timed-cycle-pause '(:walk-cycle))
	   (push-sound :step))
	 (nilf (player-acc-dir p))))

      (if (or (key-held? input :z) (joy-held? input :a))
	  (fnf p #'player-jump)
	  (nilf (player-jumping? p))))

    (when (or (key-pressed? input :a) (joy-pressed? input :y))
      (fnf (player-gun-name-cycle p) #'cycle-previous))

    (when (or (key-pressed? input :s) (joy-pressed? input :x))
      (fnf (player-gun-name-cycle p) #'cycle-next))
    p))

(defmethod input ((p player) input)
  (player-input p input))

(defun player-stage-collision (p stage)
  (let (new-ground-tile
	(collision-rects *player-collision-rectangles-alist*)
	(p (copy-structure p)))
    (aupdatef
     (player-physics p)
     (lambda (kin-2d)
       (setf (kin-2d-pos kin-2d)
	     (stage-collisions
	      (kin-2d-pos kin-2d)
	      stage
	      collision-rects
	      (let ((stop-x
		     (collision-lambda (tile-type)
		       (setf (kin-2d-vel kin-2d)
			     (zero-v :y
				     (y
				      (kin-2d-vel
				       kin-2d)))))))
		(alist :bottom
		       (collision-lambda (tile-type)
			 (setf (kin-2d-vel kin-2d)
			       (zero-v :x
				       (x
					(kin-2d-vel
					 kin-2d))))
			 (unless (player-ground-tile p)
			   (push-sound :land))
			 (setf new-ground-tile
			       tile-type))
		       :left stop-x :right stop-x :top
		       (collision-lambda (tile-type)
			 (when
			     (minusp
			      (y (kin-2d-vel kin-2d)))
			   (push-sound :head-bump))
			 (setf (kin-2d-vel kin-2d)
			       (make-v
				(x
				 (kin-2d-vel kin-2d))
				(max
				 (y
				  (kin-2d-vel kin-2d))
				 0))))))
	      (player-ground-tile p)))
       kin-2d)
     '(:stage))

    (setf (player-ground-tile p) new-ground-tile)
    (unless (player-ground-tile p)
      (aupdatef (player-timers p) #'timed-cycle-pause '(:walk-cycle))
      (nilf (player-ground-tile p)))
    p))

(defmethod stage-collision ((p player) stage)
  (player-stage-collision p stage))

(defun player-drawing (p)
  (let ((kin-2d (cdr (assoc :stage (player-physics p)))))
    (make-sprite-drawing :layer :player
			 :sheet-key :my-char
			 :src-rect (player-sprite-rect (player-h-facing p)
						       (player-actual-v-facing p)
						       (player-interacting? p)
						       (player-walking? p)
						       (player-walk-idx p)
						       (y (kin-2d-vel kin-2d)))
			 :pos (pixel-v (kin-2d-pos kin-2d)))))

(defun player-and-gun-drawing (p)
  (let ((kin-2d (cdr (assoc :stage (player-physics p)))))
    (unless
	(and (timer-active? (aval (player-timers p) :invincible))
	     (plusp (chunk-timer-period
		     (aval (player-timers p) :invincible) 50)))
      (list
       (player-drawing p)
       (player-gun-drawing (kin-2d-pos kin-2d)
			   (player-current-gun-name p)
			   (player-h-facing p)
			   (player-actual-v-facing p)
			   (player-walking? p)
			   (player-walk-idx p))))))

(defmethod draw ((p player))
  (player-and-gun-drawing p))

(defun player-vel (p)
  (kin-2d-vel (cdr (assoc :stage (player-physics p)))))

(defun player-damage-collision-rect (p)
  (rect-offset *player-damage-rect* (physics-pos p)))

(defun player-current-gun-name (p)
  (cycle-current (player-gun-name-cycle p)))
