(in-package :cave-story)

(def-entity player
    (h-facing
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

  (create-default-player (hud projectile-groups damage-numbers gun-exps active-systems)
			 (let ((id (gen-entity-id)))
			   (values
			    (make-player
			     :h-facing :left
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
			     (alist
			      :walk-cycle (create-timed-cycle 12 #(0 1 0 2) t)
			      :invincible (create-expiring-timer (s->ms 3)))

			     :physics
			     (list
			      (cons
			       :stage
			       (make-kin-2d :pos (scale-v window-dims 1/2)
					    :vel (zero-v)
					    :clamper-vx
					    (clamper+- player-max-speed-x)
					    :clamper-vy
					    (clamper+- terminal-speed)))))
			    id)))

  :timers :input :physics :stage-collision :drawable)

(defun player-pickup (p pickup)
  (ecase (pickup-type pickup)
    (:dorito
     (player-gun-exp p (pickup-amt pickup))))
  p)

(defun player-state (p)
  (estate p))

(player-methodf ai (p ticks)
  (when (and (find :walk-cycle ticks)
	     (/= 0 (timed-cycle-current (aval timers :walk-cycle))))
    (push-sound :step))

  (aupdatef
   physics
   (lambda (kin-2d)
     (modify-kin-2d (kin-2d)
       (setf accelerator-x
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
		      (t 0)))))))
       (setf accelerator-y
	     (const-accelerator
	      (if (and (minusp (y vel))
		       jumping?)
		  player-jump-gravity-acc
		  gravity-acc)))
       (setf inertia-vel
	     (if (eq ground-tile :dynamic)
		 (inertia-vel (estate ground-inertia-entity))
		 nil))

       (draw-line pos
		  (+v pos
		      (*v vel debug-velocity-scale))
		  *magenta*))) '(:stage)))

(player-method dead? (p)
  (<= health-amt 0))

(defun player-fire-gun (p)
  (let ((gun-name (player-current-gun-name (player-gun-name-cycle p))))
    (let ((num-projectile-groups (projectile-groups-count (estate (player-projectile-groups p)) gun-name))
	  (nozzle-pos (player-nozzle-pos p))
	  (dir (aif (player-actual-v-facing (player-v-facing p) (player-on-ground? (player-ground-tile p)))
		    it
		    (player-h-facing p)))
	  (lvl (gun-level (gun-exp-for (estate (player-gun-exps p)) gun-name) (cdr (assoc gun-name *gun-level-exps*))))
	  (max-projectiles (cdr (assoc gun-name *max-projectile-groups*))))
      (unless (null max-projectiles)
	(when (< num-projectile-groups max-projectiles)
	  (replace-entity-state (player-projectile-groups p)
				(rcurry #'projectile-groups-add
					(make-projectile-group gun-name lvl dir nozzle-pos))))))))

(defun player-take-damage (p dmg-amt)
  (modify-player (p)
    (unless (timer-active? (aval timers :invincible))
      (cond
	((>= (abs dmg-amt) health-amt)
	 (push-sound :player-die)
	 (stop-music)
	 (setf health-amt 0)
	 (replace-entity-state active-systems #'active-systems-switch-to-dialog)
	 (let ((entity-system-type :dialog))
	   (comment-code
	     (create-callback-timer
	      (s->ms 1/2)
	      (create-game-over-event)))))
	(t
	 (aupdatef timers #'reset-timer '(:invincible))
	 (nilf ground-tile)
	 (push-sound :hurt)
	 (replace-entity-state hud #'hud-health-changed)
	 (decf health-amt dmg-amt)
	 (replace-entity-state
	  gun-exps
	  (lambda (g)
	    (incr-gun-exp
	     g
	     (player-current-gun-name gun-name-cycle)
	     (- (* 2 dmg-amt)))))
	 (update-damage-number-amt damage-numbers id dmg-amt)
	 (aupdatef
	  physics
	  (lambda (kin-2d)
	    (modify-kin-2d (kin-2d)
	      (setf vel (make-v (x vel) (min (y vel) (- player-hop-speed)))))) '(:stage)))))))

(defmethod origin ((p player))
  (physics-tile-origin p))

(defun player-gun-exp (p amt)
  (modify-player (p)
    (replace-entity-state
     gun-exps
     (lambda (g)
       (incr-gun-exp g (player-current-gun-name (player-gun-name-cycle p)) amt)))
    (replace-entity-state hud #'hud-exp-changed)))

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
  (timed-cycle-current (aval (player-timers player) :walk-cycle)))

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


(defun player-jump (p)
  (modify-player (p)
    (unless jumping?
      (when (player-on-ground? ground-tile)
	(aupdatef
	 physics
	 (lambda (kin-2d)
	   (modify-kin-2d (kin-2d)
	     (+vf vel (make-v 0 (- player-jump-speed))))) '(:stage))

	(push-sound :jump))
      (nilf interacting? ground-tile)
      (aupdatef timers #'timed-cycle-pause '(:walk-cycle))
      (tf jumping?))))

(defun player-move (p dir)
  "Moves the player in a horizontal direction."
  (modify-player (p)
    (when (player-on-ground? ground-tile)
      (aupdatef timers #'timed-cycle-resume '(:walk-cycle))
      (when (not acc-dir)
	(aupdatef timers #'timed-cycle-restart '(:walk-cycle))))

    (nilf interacting?)
    (allf dir acc-dir h-facing)))

(defmethod input ((p player) input)
  (let ((left?  (or (key-held? input :left) (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right) (eq :positive (input-joy-axis-x input))))
	(down?  (or (key-held? input :down) (eq :positive (input-joy-axis-y input))))
	(up?    (or (key-held? input :up) (eq :negative (input-joy-axis-y input)))))

    (let* ((on-ground? (player-on-ground? (player-ground-tile p)))
	   (walking? (player-walking? (player-acc-dir p) on-ground?)))
      (modify-player (p)
	(cond
	  ;; Look Up/Down or Interact
	  ((and up? (not down?))
	   (nilf interacting?)
	   (setf v-facing :up))

	  ((and down? (not up?))
	   (unless (eq v-facing :down)
	     (setf v-facing :down)
	     (aupdatef timers #'timed-cycle-pause '(:walk-cycle))
	     (setf interacting? on-ground?)))

	  (t (nilf v-facing))))

      (cond
	;; Walk/Look based on horizontal direction
	((and left? (not right?))
	 (fnf p (rcurry #'player-move :left)))

	((and right? (not left?))
	 (fnf p (rcurry #'player-move :right)))

	(t
	 (modify-player (p)
	   (when walking?
	     (aupdatef timers #'timed-cycle-pause '(:walk-cycle))
	     (push-sound :step))
	   (nilf acc-dir))))

      (if (or (key-held? input :z) (joy-held? input :a))
	  (fnf p #'player-jump)
	  (modify-player (p)
	    (nilf jumping?))))

    (modify-player (p)
      (when (or (key-pressed? input :a) (joy-pressed? input :y))
	(fnf gun-name-cycle #'cycle-previous))

      (when (or (key-pressed? input :s) (joy-pressed? input :x))
	(fnf gun-name-cycle #'cycle-next)))))

(player-methodf stage-collision (p stage)
  (let (new-ground-tile
	(collision-rects player-collision-rectangles-alist))
    (astage-collisionsf
     (let ((stop-x
	    (collision-lambda
	      (setf vel (zero-v :y (y vel))))))
       (alist :bottom
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
		(setf vel (make-v (x vel) (max (y vel) 0))))))
     ground-tile)

    (setf ground-tile new-ground-tile)
    (unless ground-tile
      (aupdatef timers #'timed-cycle-pause '(:walk-cycle))
      (nilf ground-tile))))

(player-method draw (p)
  (let ((kin-2d (cdr (assoc :stage physics))))
    (with-kin-2d-slots (kin-2d)
      (unless (and (timer-active? (aval timers :invincible))
		   (plusp (chunk-timer-period (aval timers :invincible) 50)))
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
	   walk-idx))))))

(defun player-vel (p)
  (kin-2d-vel (cdr (assoc :stage (player-physics p)))))

(defun player-damage-collision-rect (p)
  (rect-offset player-damage-rect (physics-pos p)))

(defun player-current-gun-name (cycle)
  (cycle-current cycle))
