(in-package :cave-story)

(defstruct player
  (h-facing :left)
  v-facing
  (walk-cycle
   (create-anim-cycle
    :fps 12
    :seq #(0 1 0 2)
    :start-paused? t
    :callback
    (lambda (cycle)
      (when (/= 0 (cycle-current cycle))
	(push-sound :step)))))

  interacting?
  (pos (v/2 window-dims))
  (vel (zero-v))
  acc-dir
  ground-tile
  ground-inertia
  jumping?

  (health-amt 3)
  (max-health-amt 3)

  (invincible-timer (create-expiring-timer (s->ms 3) nil))

  hud-take-damage-fn
  gun-exp-fn
  (gun-name-cycle (create-cycle gun-names)))

(defun player-pickup (p pickup)
  (ecase (pickup-type pickup)
    (:dorito
     (awhen (player-gun-exp-fn p)
       (funcall it (pickup-amt pickup))))))

(defun player-dead? (p)
  (<= (player-health-amt p) 0))

(defun create-player (hud-take-damage-fn gun-exp-for-fn)
  (let* ((player (make-player :hud-take-damage-fn hud-take-damage-fn))
	 (dead?-fn (lambda () (player-dead? player))))
    (setf (player-gun-exp-fn player)
	  (lambda (amt)
	    (funcall gun-exp-for-fn (player-current-gun-name player) amt)))
    (def-entity-physics
	(()
	 (player-physics player)))

    (def-entity-stage-collision
	((stage)
	 (player-collisions player stage)))

    (def-entity-drawable
	(()
	 (player-draw player)))
    player))

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

(defun player-on-ground? (p)
  (player-ground-tile p))

(defun player-actual-v-facing (p)
  "The player cannot actually face down when on the ground."
  (let ((vf (player-v-facing p)))
    (if (and (player-on-ground? p)
	     (eq vf :down))
	nil
	vf)))

(defun player-walk-idx (player)
  (anim-cycle-current (player-walk-cycle player)))

(defun player-walking? (player)
  (and (player-acc-dir player) (player-on-ground? player)))

(defun player-sprite-rect (p)
  "The sprite-rect for player P."
  (tile-rect (char-sprite-pos
	      (player-h-facing p)
	      (player-actual-v-facing p)
	      (player-interacting? p)
	      (cond
		((player-walking? p)
		 (player-walk-idx p))
		((plusp (y (player-vel p))) 1)
		((minusp (y (player-vel p))) 2)
		(t 0)))))


(defun player-jump (player)
  (unless (player-jumping? player)
    (when (player-on-ground? player)
      (setf (y (player-vel player)) (- player-jump-speed))
      (push-sound :jump))
    (nilf (player-interacting? player)
	  (player-ground-tile player))
    (anim-cycle-pause (player-walk-cycle player))
    (tf (player-jumping? player))))

(defun player-move (player dir)
  "Moves the player in a horizontal direction."
  (when (player-on-ground? player)
    (anim-cycle-resume (player-walk-cycle player))
    (when (not (player-acc-dir player))
      (anim-cycle-reset (player-walk-cycle player))))
  (setf (player-acc-dir player) dir)
  (setf (player-h-facing player) dir)
  (nilf (player-interacting? player)))

(defun player-input (player input)
  (let ((left?  (or (key-held? input :left) (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right) (eq :positive (input-joy-axis-x input))))
	(down?  (or (key-held? input :down) (eq :positive (input-joy-axis-y input))))
	(up?    (or (key-held? input :up) (eq :negative (input-joy-axis-y input)))))
    (cond
      ;; Look Up/Down or Interact
      ((and up? (not down?))
       (nilf (player-interacting? player))
       (setf (player-v-facing player) :up))
      ((and down? (not up?))
       (unless (eq (player-v-facing player) :down)
	 (setf (player-v-facing player) :down)
	 (anim-cycle-pause (player-walk-cycle player))
	 (setf (player-interacting? player)
	       (player-on-ground? player))))
      (t (nilf (player-v-facing player))))
    (cond
      ;; Walk/Look based on horizontal direction
      ((and left? (not right?))
       (player-move player :left))
      ((and right? (not left?))
       (player-move player :right))
      (t
       (when (player-walking? player)
	 (anim-cycle-pause (player-walk-cycle player))
	 (push-sound :step))
       (nilf (player-acc-dir player))))


    (if (or (key-held? input :z) (member 0 (input-held-joy-buttons input)))
	(player-jump player)
	(nilf (player-jumping? player)))))

(defun player-physics (player)
  (let ((acc-y
	 ;; Vertical motion
	 (const-accelerator
	  (if (and (minusp (y (player-vel player)))
		   (player-jumping? player))
	      player-jump-gravity-acc
	      gravity-acc)))
	(acc-x
	 ;; Horizontal motion
	 (cond
	   ((and (player-on-ground? player)
		 (null (player-acc-dir player)))
	    ;; Not accelerating, apply friction.
	    (friction-accelerator player-friction-acc))
	   (t
	    ;; Apply walking accelerations.
	    (const-accelerator
	     (* (if (player-on-ground? player)
		    player-walk-acc
		    player-air-acc)
		(case (player-acc-dir player)
		  (:left -1)
		  (:right 1)
		  (t 0))))))))

    (physics-2d
     (player-pos player)
     (player-vel player)
     acc-x acc-y
     :clamper-vx
     (clamper+- player-max-speed-x)
     :clamper-vy
     (clamper+- terminal-speed)))

  (when (eq (player-ground-tile player) :dynamic)
    (awhen (player-ground-inertia player)
      (incf (x (player-pos player)) (accelerate (x (funcall it)) 0))))

  (draw-line (player-pos player)
	     (+v (player-pos player)
		 (*v (player-vel player) debug-velocity-scale))
	     magenta))

(defun player-collisions (player stage)
  (let (ground-tile
	(stop-x
	 (collision-lambda
	   (setf (x (player-vel player)) 0))))

    (stage-collisions ((player-pos player) player-collision-rectangles-alist stage (player-ground-tile player))
      :bottom
      (collision-lambda
	(setf (y (player-vel player)) 0)
	(unless (player-ground-tile player)
	  (push-sound :land))
	(setf ground-tile tile-type))

      :left stop-x
      :right stop-x

      :top
      (collision-lambda
	(when (minusp (y (player-vel player)))
	  (push-sound :head-bump))
	(maxf (y (player-vel player)) 0)))

    (setf (player-ground-tile player) ground-tile)
    (unless ground-tile
      (anim-cycle-pause (player-walk-cycle player))
      (nilf (player-ground-tile player)))))

(defun player-draw (player)
  (let ((invincible-timer (player-invincible-timer player)))
    (unless (and (timer-active? invincible-timer)
		 (plusp (chunk-time-period invincible-timer 50)))
      (draw-sprite :player :my-char (player-sprite-rect player) (pixel-v (player-pos player)))
      (player-draw-gun player))))

(defun player-damage-collision-rect (p)
  (rect-offset player-damage-rect (player-pos p)))

(defun player-current-gun-name (p)
  (cycle-current (player-gun-name-cycle p)))

(defun player-fire-gun (player gun-exps)
  (let ((gun-name (player-current-gun-name player)))
    (let ((num-projectile-groups (count gun-name projectile-groups :key #'car))
	  (nozzle-pos (player-nozzle-pos player))
	  (dir (aif (player-actual-v-facing player)
		    it
		    (player-h-facing player)))
	  ;; TODO: Determine the level of the gun.
	  (lvl (gun-level (cdr (assoc gun-name gun-exps)) (cdr (assoc gun-name gun-level-exps))))
	  (max-projectiles (cdr (assoc gun-name max-projectile-groups))))
      (unless (null max-projectiles)
	(when (< num-projectile-groups max-projectiles)
	  (push (make-projectile-group gun-name lvl dir nozzle-pos)
		projectile-groups))))))

(defun player-take-damage (p dmg-amt)
  (unless (timer-active? (player-invincible-timer p))
    (cond
      ((>= (abs dmg-amt) (player-health-amt p))
       (push-sound :player-die)
       (switch-to-new-song :gameover)
       (setf (player-health-amt p) 0)
       (tf paused?))
      (t
       (reset-timer (player-invincible-timer p))
       (nilf (player-ground-tile p))
       (push-sound :hurt)
       (funcall (player-hud-take-damage-fn player))
       (decf (player-health-amt p) dmg-amt)
       (funcall (player-gun-exp-fn p) (- (* 2 dmg-amt)))
       (update-damage-number-amt p (lambda () (+v (tile-dims/2) (player-pos p))) dmg-amt)
       (minf (y (player-vel p)) (- player-hop-speed))))))
