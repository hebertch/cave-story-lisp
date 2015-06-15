(in-package :cave-story)

(defstructure player
    (h-facing :left)
  v-facing
  interacting?
  (pos (v/2 window-dims))
  (vel (zero-v))
  acc-dir
  ground-tile
  jumping?
  (gun-name-cycle (create-cycle gun-names))
  (invincible-timer (create-expiring-timer (s->ms 3)))
  (walk-cycle (create-timed-cycle 12 #(0 1 0 2) t))
  (health-amt 3)
  (max-health-amt 3)

  ground-inertia-entity
  damage-numbers
  gun-exps
  projectile-groups)

(defun player-pickup (p pickup)
  (ecase (pickup-type pickup)
    (:dorito
     (ecall p :gun-exp (pickup-amt pickup)))))

(defun player-state (p)
  (ecall p :state))

(defun player-dead? (p)
  (ecall p :dead?))

(defun create-default-player (hud projectile-groups damage-numbers gun-exps active-systems &key (id (gen-entity-id)))
  (let* ((p (make-player :damage-numbers damage-numbers :gun-exps gun-exps :projectile-groups projectile-groups))
	 (dead?-fn (lambda () (<= (player-health-amt p) 0))))

    (def-entity-timer
	(()
	 (setf p (with-player-copy-slots (p)
		   (fnf invincible-timer #'update-timer)
		   p))))

    (def-entity-timer
	(()
	 (setf p
	       (with-player-copy-slots (p)
		 (mvbind (tc ticked?) (update-timed-cycle walk-cycle)
		   (when (and ticked? (/= 0 (timed-cycle-current walk-cycle)))
		     (push-sound :step))
		   (setf walk-cycle tc))
		 p))))

    (def-entity-input
	((input)
	 (fnf p (rcurry #'player-input input))))
    (def-entity-physics
	(()
	 (fnf p #'player-physics)))

    (def-entity-stage-collision
	((stage)
	 (setf p (player-collisions p stage))))

    (def-entity-drawable
	(()
	 (player-draw p)))

    (register-entity-interface
     id
     (dlambda
      (:state () (copy-player p))
      (:dead? () (funcall dead?-fn))
      (:origin () (+v (player-pos p) (tile-dims/2)))
      (:dynamic-collision (new-p) (setf p new-p))
      (:gun-exp (amt)
		(ecall gun-exps :incr (player-current-gun-name (player-gun-name-cycle p)) amt)
		(ecall hud :exp-changed))
      (:take-damage (dmg-amt)
		    (setf
		     p
		     (with-player-copy-slots (p)
		       (unless (timer-active? invincible-timer)
			 (cond
			   ((>= (abs dmg-amt) health-amt)
			    (push-sound :player-die)
			    (stop-music)
			    (setf health-amt 0)
			    (ecall active-systems :switch-to-dialog)
			    (let ((entity-system-type :dialog))
			      (create-callback-timer
			       (s->ms 1/2)
			       (lambda ()
				 (switch-to-new-song :gameover)
				 (create-text-display (tiles/2-v 7 24) "You have died.")))))
			   (t
			    (fnf invincible-timer #'reset-timer)
			    (nilf ground-tile)
			    (push-sound :hurt)
			    (ecall hud :health-changed)
			    (decf health-amt dmg-amt)
			    (ecall gun-exps :incr (player-current-gun-name gun-name-cycle) (- (* 2 dmg-amt)))
			    (update-damage-number-amt damage-numbers id dmg-amt)
			    (setf vel (make-v (x vel) (min (y vel) (- player-hop-speed)))))))
		       p)))

      (:fire-gun ()
		 (let ((gun-name (player-current-gun-name (player-gun-name-cycle p))))
		   (let ((num-projectile-groups (ecall projectile-groups :count gun-name))
			 (nozzle-pos (player-nozzle-pos p))
			 (dir (aif (player-actual-v-facing (player-v-facing p) (player-on-ground? (player-ground-tile p)))
				   it
				   (player-h-facing p)))
			 ;; TODO: Determine the level of the gun.
			 (lvl (gun-level (ecall gun-exps :exp-for gun-name) (cdr (assoc gun-name gun-level-exps))))
			 (max-projectiles (cdr (assoc gun-name max-projectile-groups))))
		     (unless (null max-projectiles)
		       (when (< num-projectile-groups max-projectiles)
			 (ecall projectile-groups :add (make-projectile-group gun-name lvl dir nozzle-pos)))))))))))

(defparameter player-walk-acc 0.00083007812)
(defparameter player-max-speed-x 0.15859375)
(defparameter player-friction-acc 0.00049804687)
(defparameter terminal-speed 0.2998046875)
(defparameter gravity-acc 0.00078125)
(defparameter player-jump-gravity-acc 0.0003125)
(defparameter player-air-acc 0.0003125)
(defparameter player-jump-speed 0.25)
(defparameter player-hop-speed (/ player-jump-speed 1.5))

(defparameter player-collision-rectangles-alist
  (loop for (key x y w h) in
       '((:bottom 11 16 10 15)
	 (:top 7 2 18 15)
	 (:left 6 8 10 12)
	 (:right 16 8 10 12))
     collect (cons key (make-rect :pos (make-v x y) :size (make-v w h)))))

(defun player-collision-rect (side)
  (cdr (assoc side player-collision-rectangles-alist)))

(defparameter player-damage-rect
  (let ((left (left (player-collision-rect :left)))
	(right (right (player-collision-rect :right)))
	(top (top (player-collision-rect :top)))
	(bottom (bottom (player-collision-rect :bottom))))
    (make-rect :pos (make-v left top)
	       :size (make-v (- right left)
			     (- bottom top)))))

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

(defun player-on-ground? (ground-tile)
  ground-tile)

(defun player-actual-v-facing (v-facing on-ground?)
  "The player cannot actually face down when on the ground."
  (if (and on-ground? (eq v-facing :down))
      nil
      v-facing))

(defun player-walk-idx (player)
  (timed-cycle-current (player-walk-cycle player)))

(defun player-walking? (acc-dir on-ground?)
  (and acc-dir on-ground?))

(defun player-sprite-rect (h-facing actual-v-facing interacting? walking? walk-idx vel-y)
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


(defun player-jump (jumping? on-ground? vel interacting? ground-tile walk-cycle)
  (unless jumping?
    (when on-ground?
      (+vf vel (make-v 0 (- player-jump-speed)))
      (push-sound :jump))
    (nilf interacting? ground-tile)
    (fnf walk-cycle #'timed-cycle-pause)
    (tf jumping?))
  (values interacting? ground-tile jumping? vel walk-cycle))

(defun player-move (on-ground? walk-cycle acc-dir dir)
  "Moves the player in a horizontal direction."
  (when on-ground?
    (fnf walk-cycle #'timed-cycle-resume)
    (when (not acc-dir)
      (fnf walk-cycle #'timed-cycle-restart)))

  (setf acc-dir dir)
  (let ((acc-dir dir)
	(h-facing dir)
	(interacting? nil))
    (values acc-dir h-facing interacting? walk-cycle)))

(defun player-input (p input)
  (let ((left?  (or (key-held? input :left) (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right) (eq :positive (input-joy-axis-x input))))
	(down?  (or (key-held? input :down) (eq :positive (input-joy-axis-y input))))
	(up?    (or (key-held? input :up) (eq :negative (input-joy-axis-y input)))))
    (with-player-copy-slots (p)
      (let* ((on-ground? (player-on-ground? ground-tile))
	     (walking? (player-walking? acc-dir on-ground?)))
	(cond
	  ;; Look Up/Down or Interact
	  ((and up? (not down?))
	   (nilf interacting?)
	   (setf v-facing :up))

	  ((and down? (not up?))
	   (unless (eq v-facing :down)
	     (setf v-facing :down)
	     (fnf walk-cycle #'timed-cycle-pause)
	     (setf interacting? on-ground?)))

	  (t (nilf v-facing)))

	(cond
	  ;; Walk/Look based on horizontal direction
	  ((and left? (not right?))
	   (mvsetq (acc-dir h-facing interacting? walk-cycle) (player-move on-ground? walk-cycle acc-dir :left)))

	  ((and right? (not left?))
	   (mvsetq (acc-dir h-facing interacting? walk-cycle) (player-move on-ground? walk-cycle acc-dir :right)))

	  (t
	   (when walking?
	     (fnf walk-cycle #'timed-cycle-pause)
	     (push-sound :step))
	   (nilf acc-dir)))

	(if (or (key-held? input :z) (joy-held? input :a))
	    (mvsetq (interacting? ground-tile jumping? vel walk-cycle)
		    (player-jump jumping? on-ground? vel interacting? ground-tile walk-cycle))
	    (nilf jumping?)))

      (when (or (key-pressed? input :a) (joy-pressed? input :y))
	(fnf gun-name-cycle #'cycle-previous))

      (when (or (key-pressed? input :s) (joy-pressed? input :x))
	(fnf gun-name-cycle #'cycle-next))
      p)))

(defun player-physics (p)
  (with-player-copy-slots (p)
    (let ((acc-y
	   ;; Vertical motion
	   (const-accelerator
	    (if (and (minusp (y vel))
		     jumping?)
		player-jump-gravity-acc
		gravity-acc)))
	  (acc-x
	   ;; Horizontal motion
	   (cond
	     ((and (player-on-ground? ground-tile)
		   (null acc-dir))
	      ;; Not accelerating, apply friction.
	      (friction-accelerator player-friction-acc))
	     (t
	      ;; Apply walking accelerations.
	      (const-accelerator
	       (* (if (player-on-ground? ground-tile)
		      player-walk-acc
		      player-air-acc)
		  (case acc-dir
		    (:left -1)
		    (:right 1)
		    (t 0))))))))

      (physics-2d
       pos
       vel
       acc-x acc-y
       :clamper-vx
       (clamper+- player-max-speed-x)
       :clamper-vy
       (clamper+- terminal-speed)))

    (when (eq ground-tile :dynamic)
      (awhen ground-inertia-entity
	(incf (x pos) (accelerate (x (ecall it :vel)) 0))))

    (draw-line pos
	       (+v pos
		   (*v vel debug-velocity-scale))
	       magenta)
    p))

(defun player-collisions (p stage)
  (with-player-copy-slots (p)
    (let (new-ground-tile
	  (stop-x
	   (collision-lambda
	     (setf vel (zero-v :y (y vel))))))

      (stage-collisions (pos player-collision-rectangles-alist stage ground-tile)
	:bottom
	(collision-lambda
	  (setf vel (zero-v :x (x vel)))
	  (unless ground-tile
	    (push-sound :land))
	  (setf new-ground-tile tile-type))

	:left stop-x
	:right stop-x

	:top
	(collision-lambda
	  (when (minusp (y vel))
	    (push-sound :head-bump))
	  (setf vel (make-v (x vel) (max (y vel) 0)))))

      (setf ground-tile new-ground-tile)
      (unless ground-tile
	(fnf walk-cycle #'timed-cycle-pause)
	(nilf ground-tile))

      p)))

(defun player-draw (p)
  (with-player-slots (p)
    (unless (and (timer-active? invincible-timer)
		 (plusp (chunk-timer-period invincible-timer 50)))
      (let* ((on-ground? (player-on-ground? ground-tile))
	     (actual-v-facing (player-actual-v-facing v-facing on-ground?))
	     (walking? (player-walking? acc-dir on-ground?))
	     (walk-idx (player-walk-idx p)))
	(draw-sprite
	 :player
	 :my-char
	 (player-sprite-rect
	  h-facing
	  actual-v-facing
	  interacting?
	  walking?
	  walk-idx
	  (y vel))
	 (pixel-v pos))

	(player-draw-gun
	 pos
	 (player-current-gun-name gun-name-cycle)
	 h-facing
	 actual-v-facing
	 walking?
	 walk-idx)))))

(defun player-damage-collision-rect (p)
  (rect-offset player-damage-rect (player-pos p)))

(defun player-current-gun-name (cycle)
  (cycle-current cycle))

(defun player-fire-gun (p)
  (ecall p :fire-gun))

(defun player-die (p)
  (push-sound :player-die)
  (stop-music)
  (setf (player-health-amt p) 0)
  (setf active-update-systems (list :dialog))
  (setf active-input-system :dialog)
  (push :dialog active-draw-systems)
  (let ((entity-system-type :dialog))
    (create-callback-timer
     (s->ms 1/2)
     (lambda ()
       (switch-to-new-song :gameover)
       (create-text-display (tiles/2-v 7 24) "You have died.")))))

(defun player-take-damage (p dmg-amt)
  (ecall p :take-damage dmg-amt))
