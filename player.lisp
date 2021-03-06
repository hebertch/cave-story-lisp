(in-package :cave-story)

(defvar* *player-invincible* nil)

(defun player-fns-alist ()
  (alist :stage-collision-fn #'player-stage-collision
	 :ai-fn #'player-ai
	 :origin-fn #'physics-tile-origin
	 :input-fn #'player-input
	 :draw-fn #'player-and-gun-drawing))

(defvar* *player-subsystems*
    '(:timers :input :physics :stage-collision :drawable))

(defun make-player (&key pos)
  (amerge
   (player-fns-alist)
   (alist :subsystems *player-subsystems*)
   (alist :h-facing :left
	  :persistence :indefinite
	  :gun-name-cycle (make-cycle :seq *gun-names*)
	  :health-amt 3
	  :max-health-amt 3
	  :id (gen-entity-id)
	  :timers '(:walk-cycle :invincible-timer)
	  :walk-cycle
	  (make-fps-cycle 12 #(0 1 0 2) t)
	  :invincible-timer
	  (make-expiring-timer (s->ms 3))
	  :physics '(:stage-physics)
	  :stage-physics
	  (make-kin-2d
	   :pos pos
	   :vel (zero-v)
	   :clamper-vx
	   (clamper+- *player-max-speed-x*)
	   :clamper-vy
	   (clamper+- *terminal-speed*)))))

(defun set-player-pos (player pos)
  (aupdate player :stage-physics (asetfn :pos pos)))

(defvar* *player-walk-acc* 0.00083007812)
(defvar* *player-max-speed-x* 0.15859375)
(defvar* *player-max-speed-x-water* (/ *player-max-speed-x* 2))
(defvar* *player-friction-acc* 0.00049804687)
(defvar* *terminal-speed* 0.2998046875)
(defvar* *gravity-acc* 0.00078125)
(defvar* *player-jump-gravity-acc* 0.0003125)
(defvar* *player-air-acc* 0.0003125)
(defvar* *player-jump-speed* 0.25)
(defvar* *player-jump-speed-water* (* *player-jump-speed* 3/4))
(defvar* *player-hop-speed* (* *player-jump-speed* 2/3))

(defvar* *player-collision-rectangles-alist*
    (loop for (key x y w h) in
	 '((:bottom 11 16 10 15)
	   (:top 7 2 18 15)
	   (:left 6 10 10 14)
	   (:right 16 10 10 15))
       collect (cons key (make-rect :pos (make-v x y) :size (make-v w h)))))

(defun player-collision-rect (side)
  (aval *player-collision-rectangles-alist* side))

(defvar* *player-damage-rect*
    (let ((left (left (player-collision-rect :left)))
	  (right (right (player-collision-rect :right)))
	  (top (top (player-collision-rect :top)))
	  (bottom (bottom (player-collision-rect :bottom))))
      (make-rect :pos (make-v left top)
		 :size (make-v (- right left)
			       (- bottom top)))))

(defun player-pickup (p pickup)
  (ecase (aval pickup :type)
    (:dorito
     (player-gun-exp p (aval pickup :amt)))
    (:heart
     (player-add-health p (aval pickup :amt)))
    (:missile p)))

(defun player-add-health (p amt)
  (aupdate p
	   :health-amt (lambda (old-amt)
			 (min (aval p :max-health-amt) (+ old-amt amt)))
	   :new-states
	   (pushfn
	    (hud-health-changed
	     (estate (entity-id :hud))))))

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
  (draw-line (aval kin-2d :pos)
	     (+ (aval kin-2d :pos)
		(* (aval kin-2d :vel) *debug-velocity-scale*))
	     *magenta*)

  (aset kin-2d
	:accelerator-x (player-accelerator-x p)
	:accelerator-y (player-accelerator-y p kin-2d)
	:clamper-vx
	(clamper+- (if (player-in-water? p)
		       *player-max-speed-x-water*
		       *player-max-speed-x*))
	:inertia-vel
	(player-inertia-vel p (estate (aval p :ground-inertia-entity)))))

(defun apply-player-physics (p)
  (aupdate p
	   :stage-physics
	   #_(player-kin-2d-physics p _)))

(setfn player-ai
       (lambda (p)
	 (if (and (ticked? p :walk-cycle)
		  (/= 0 (cycle-current (aval p :walk-cycle))))
	     (aupdate p :sound-effects (pushfn :snd-player-walk))
	     p))
       apply-player-physics)

(defun player-fire-gun (p)
  (let ((gun-name (player-current-gun-name p)))
    (let ((num-projectile-groups
	   (projectile-groups-count (entity :projectile-groups) gun-name))
	  (nozzle-pos (player-nozzle-pos p))
	  (dir (if (player-actual-v-facing p)
		   (player-actual-v-facing p)
		   (aval p :h-facing)))
	  (lvl (gun-level (gun-exp-for (entity :gun-exps) gun-name)
			  (cdr (assoc gun-name *gun-level-exps*))))
	  (max-projectiles (cdr (assoc gun-name *max-projectile-groups*))))
      (if (and (not (null max-projectiles))
	       (< num-projectile-groups max-projectiles))
	  (add-projectile-group p gun-name lvl dir nozzle-pos)
	  p))))

(defun player-short-hop-physics (kin-2d)
  (let ((vel (aval kin-2d :vel)))
    (aset kin-2d
	  :vel
	  (make-v (x vel)
		  (min (y vel)
		       (- *player-hop-speed*))))))

(defun player-take-damage (p dmg-amt)
  (call-if
   (when (not (or *player-invincible*
		  (timer-active? (aval p :invincible-timer))))
     (cond
       ((>= (abs dmg-amt) (aval p :health-amt))
	(stop-music!)
	
	(aupdatefn
	 :health-amt (constantly 0)
	 :dead? (constantly t)
	 :sound-effects (pushfn :snd-player-die)))
       (t
	(comp
	 (damage-number-update-amtfn dmg-amt)
	 (aupdatefn
	  :ground-tile (constantly nil)
	  :invincible-timer #'reset-timer
	  :health-amt #_(- _ dmg-amt)
	  :sound-effects (pushfn :snd-player-hurt)
	  :stage-physics #'player-short-hop-physics
	  :new-states
	  (appendfn
	   (list (hud-health-changed
		  (estate (entity-id :hud)))
		 (incr-gun-exp
		  (estate (entity-id :gun-exps))
		  (player-current-gun-name p)
		  (- (* 2 dmg-amt))))))))))
   p))

(defun player-gun-exp (p amt)
  (aupdate p
	   :new-states
	   (appendfn
	    (list (incr-gun-exp (estate (entity-id :gun-exps))
				(player-current-gun-name p) amt)
		  (hud-exp-changed (estate (entity-id :hud))))
	    (experience-number-update :exp amt))))

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
  (cycle-current (aval player :walk-cycle)))

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

(defun player-jump-physics (kin-2d in-water?)
  (aupdate kin-2d
	   :vel
	   #_(+ _
		(make-v 0 (- (if in-water?
				 *player-jump-speed-water*
				 *player-jump-speed*))))))

(defun player-jump (p)
  (cond ((not (aval p :jumping?))
	 ;; TODO: Messy
	 (aset (if (player-on-ground? p)
		   (aupdate p
			    :stage-physics #_(player-jump-physics
					      _
					      (player-in-water? p))
			    :sound-effects (pushfn :snd-player-jump)
			    :walk-cycle #'timed-cycle-pause)
		   p)
	       :jumping? t
	       :interacting? nil
	       :ground-tile nil))
	(t p)))

(defun player-move (p dir)
  "Moves the player in a horizontal direction."
  (declare (optimize debug))
  (let ((walk-cycle
	 (if (player-on-ground? p)
	     (if (null (aval p :acc-dir))
		 (timed-cycle-resume
		  (timed-cycle-restart (aval p :walk-cycle)))
		 (timed-cycle-resume (aval p :walk-cycle)))
	     (aval p :walk-cycle))))
    (aset p
	  :walk-cycle walk-cycle
	  :interacting? nil
	  :acc-dir dir
	  :h-facing dir)))

(defun player-interact (p)
  (let ((player-tile (pos->tile-pos (aval (aval p :stage-physics) :pos)))
	(entities (entities-with-flag :script-on-activate)))
    (aset p
	  :script-entity
	  (find player-tile entities :key (part (aval _ :tile-pos)) :test #'equalp))))

(defun player-input (p input)
  (funcall (comp
	    (part (player-fire-gun-input _ input))
	    (part (player-gun-cycle-input _ input))
	    (part (player-jump-input _ input))
	    (part (player-left/right-input _ input))
	    (part (player-up/down-input _ input)))
	   p))

(defun player-jump-input (p input)
  (if (or (key-held? input :z) (joy-held? input :a))
      (player-jump p)
      (aset p :jumping? nil)))

(defun player-left/right-input (p input)
  (let ((left?  (or (key-held? input :left)
		    (eq :negative (input-joy-axis-x input))))
	(right? (or (key-held? input :right)
		    (eq :positive (input-joy-axis-x input))))
	(walking? (player-walking? p)))
    (cond
      ;; Walk/Look based on horizontal direction
      ((and left? (not right?)) (player-move p :left))
      ((and right? (not left?)) (player-move p :right))
      (t
       (let ((p2 (aset p :acc-dir nil)))
	 (if walking?
	     (aupdate p2
		      :walk-cycle #'timed-cycle-pause
		      :sound-effects (pushfn :snd-player-walk))
	     p2))))))

(defun player-up/down-input (p input)
  (let* ((down?  (or (key-held? input :down)
		     (eq :positive (input-joy-axis-y input))))
	 (up?    (or (key-held? input :up)
		     (eq :negative (input-joy-axis-y input))))
	 (on-ground? (player-on-ground? p))
	 (face-down? (and down? (not up?))))
    (cond
      ;; Look Up/Down or Interact
      ((and up? (not down?))
       (aset p
	     :interacting? nil
	     :v-facing :up))

      ((and face-down? (eq (aval p :v-facing) :down))
       p)
      (face-down?
       ;; TODO: messy
       (let ((p2 
	      (aset p
		    :v-facing :down
		    :walk-cycle
		    (timed-cycle-pause (aval p :walk-cycle))
		    :interacting? on-ground?)))
	 (if (aval p2 :interacting?)
	     (player-interact p2)
	     p2)))

      (t (aset p :v-facing nil)))))

(defun player-fire-gun-input (p input)
  (if (or (joy-pressed? input :b) (key-pressed? input :x))
      (player-fire-gun p)
      p))

(defun player-gun-cycle-input (p input)
  (let ((gun-name-cycle (aval p :gun-name-cycle)))
    (cond
      ((or (key-pressed? input :a) (joy-pressed? input :y))
       (aset p :gun-name-cycle (cycle-previous gun-name-cycle)))    
      ((or (key-pressed? input :s) (joy-pressed? input :x))
       (aset p :gun-name-cycle (cycle-next gun-name-cycle)))
      (t p))))

(defvar* *player-stage-collisions*
    (let ((stop-x
	   (collision-lambda (data)
	     (aset data
		   :stage-physics
		   (aset (aval data :stage-physics)
			 :vel (zero-v :y (y (stage-vel data))))))))
      (alist :bottom
	     (collision-lambda (data)
	       (aupdate data
			:sound-effects
			(unless (aval data :ground-tile)
			  (pushfn :snd-thud))
			:stage-physics
			(asetfn :vel (zero-v :x (x (stage-vel data))))
			:new-ground-tile (constantly (aval data :tile-type))))
	     :left stop-x :right stop-x
	     :top
	     (collision-lambda (data)
	       (aset data
		     :sound-effects
		     (if (minusp (y (stage-vel data)))
			 (cons :snd-bonk-head (aval data :sound-effects))
			 (aval data :sound-effects))
		     :stage-physics
		     (aset (aval data :stage-physics)
			   :vel (make-v
				 (x (stage-vel data))
				 (max (y (stage-vel data)) 0))))))))

(defun player-stage-collision (p stage)
  (let* ((collision-rects *player-collision-rectangles-alist*)
	 (res (stage-collisions
	       (aset p :new-ground-tile nil)
	       stage collision-rects
	       *player-stage-collisions*
	       (aval p :ground-tile))))
    (aset res
	  :ground-tile (aval res :new-ground-tile)
	  :walk-cycle (timed-cycle-pause (aval res :walk-cycle))
	  :tiles
	  (stage-get-colliding-tiles
	   stage
	   (rect-offset (aval *player-collision-rectangles-alist* :bottom)
			(physics-pos p))))))

(defun player-in-water? (p)
  (some (lambda (tile)
	  (water-tile? (second tile)))
	(aval p :tiles)))

(defun player-drawing (p)
  (let ((kin-2d (aval p :stage-physics)))
    (make-sprite-drawing
     :layer :player
     :sheet-key :my-char
     :src-rect (player-sprite-rect (aval p :h-facing)
				   (player-actual-v-facing p)
				   (aval p :interacting?)
				   (player-walking? p)
				   (player-walk-idx p)
				   (y (aval kin-2d :vel)))
     :pos (pixel-v (aval kin-2d :pos)))))

(defun player-and-gun-drawing (p)
  (let ((kin-2d (aval p :stage-physics)))
    (unless
	(and (timer-active? (aval p :invincible-timer))
	     (plusp (chunk-timer-period
		     (aval p :invincible-timer) 50)))
      (list
       (player-drawing p)
       (player-gun-drawing (aval kin-2d :pos)
			   (player-current-gun-name p)
			   (aval p :h-facing)
			   (player-actual-v-facing p)
			   (player-walking? p)
			   (player-walk-idx p))))))

(defun player-vel (p)
  (aval (aval p :stage-physics) :vel))

(defun player-damage-collision-rect (p)
  (rect-offset *player-damage-rect* (physics-pos p)))

(defun player-current-gun-name (p)
  (cycle-current (aval p :gun-name-cycle)))
