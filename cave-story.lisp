;;;; cave-story.lisp

(in-package #:cave-story)

(comment-code
  (progn
    (ql:quickload :cave-story)
    (in-package :cave-story)
    (swank:set-default-directory "/home/chebert/Projects/lisp/cave-story")))

(defvar window)
(defvar renderer)
(defvar font)

;; Debug params.
(defparameter update-period 1
  "Number of frames per update. 1 for real-time.")

(defstructure game
    player
  camera
  stage
  projectile-groups
  damage-numbers
  active-systems

  (input (make-input)))

(defun main ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       last-update-time)
	   (setq global-game (init))
	   (setf last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (fnf global-game #'handle-input)
		  (let ((input (game-input global-game)))
		    (when (and global-paused? (key-pressed? input :n))
		      (update-and-render renderer global-game))
		    (when (or (key-pressed? input :r) (joy-pressed? input :select))
		      (setq global-game (reset))))
		  (when (>= frame-timer (* update-period frame-time))
		    (unless global-paused?
		      (update-and-render renderer global-game))
		    (render renderer
			    font
			    render-list
			    (camera-focus->camera-pos
			     (clamp-pos (camera-focus (estate (game-camera global-game)))
					(stage-dims->camera-bounds (stage-dims (game-stage global-game))))))
		    (decf frame-timer (* update-period frame-time)))

		  (let ((dt (- (sdl:get-ticks) last-update-time)))
		    ;; NOTE: if we are paused beyond our control, Don't play catchup.
		    (incf frame-timer (min dt (* 2 frame-time))))
		  (setf last-update-time (sdl:get-ticks))
		  (music-update)
		  (sdl:delay 1))))
      (cleanup))))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table
	     :test (hash-table-test hash-table)
	     :rehash-size (hash-table-rehash-size hash-table)
	     :rehash-threshold (hash-table-rehash-threshold hash-table)
	     :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(let (debug-toggle-off?)
  (defun handle-input (game &aux input)
    "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."
    (modify-game (game)
      (fnf input #'gather-input))
    (setf input (game-input game))
    (when (any? (intersection (input-held-keys input) '(:capslock :escape) :test #'eq))
      (quit))
    (when (or (key-pressed? input :p) (member 7 (ti-pressed-joy-buttons (input-transient-input input))))
      (togglef global-paused?))

    (when (key-pressed? input :f1)
      (mapc (if debug-toggle-off?
		(progn
		  (nilf debug-toggle-off?)
		  #'remove-visible-layer)
		#'toggle-visible-layer)
	    debug-layers))

    (loop for key in '(:f2 :f3 :f4 :f5 :f6)
       for i from 0
       do
	 (when (key-pressed? input key)
	   (let ((layer (elt debug-layers (1+ i))))
	     (print layer)
	     (tf debug-toggle-off?)
	     (toggle-visible-layer layer))))


    (let ((input-systems (active-systems-input (estate (game-active-systems game)))))
      (update-input-subsystem input-systems input)
      (ecase (first input-systems)
	(:game
	 (when (or (joy-pressed? input :b) (key-pressed? input :x))
	   ;; Fire Gun
	   (player-fire-gun (estate (game-player game)))))

	(:dialog
	 (cond
	   ((or (joy-pressed? input :b) (key-pressed? input :x))
	    (dialog-ok-pressed))
	   ((or (joy-held? input :a) (key-pressed? input :z)
		(joy-held? input :b) (key-pressed? input :x))
	    (dialog-button-held))
	   (t
	    (dialog-buttons-released))))))
    game))

(defun dialog-ok-pressed ()
  )
(defun dialog-button-held ()
  (fast-text-speed))
(defun dialog-buttons-released ()
  (slow-text-speed))

(defun collision-rects (pos size buffer-size)
  "Creates :TOP :LEFT :RIGHT :BOTTOM rects."
  (let* ((bs buffer-size)
	 (dbs (* 2 bs)))
    (list
     (cons :top (create-rect (+v (make-v bs 0) pos)
			     (-v size (make-v dbs bs))))

     (cons :bottom (create-rect (+v (make-v bs bs) pos)
				(-v size (make-v dbs bs))))

     (cons :left (create-rect (+v (make-v 0 bs) pos)
			      (-v size (make-v bs dbs))))

     (cons :right (create-rect (+v (make-v bs bs) pos)
			       (-v size (make-v bs dbs)))))))

(defun rect->collision-rects (rect &optional (buffer-size 6))
  (collision-rects (rect-pos rect) (rect-size rect) buffer-size))

(defparameter dorito-friction-acc 0.00002)
(defparameter dorito-bounce-speed 0.225)

(defstructure pickup type amt)
(defstructure dorito
    (life-timer (create-expiring-timer (s->ms 8) t))
  (anim-cycle (create-timed-cycle 14 (alexandria:iota 6)))
  dead?
  pos
  vel
  size)

(defmethod ai ((d dorito))
  (modify-dorito (d)
    (fnf anim-cycle #'update-timed-cycle)
    (fnf life-timer #'update-timer)
    (unless (timer-active? life-timer)
      (tf dead?))))

(defmethod physics ((d dorito))
  (modify-dorito (d)
    (physics-2d
     pos
     vel
     (friction-accelerator dorito-friction-acc)
     (const-accelerator gravity-acc)
     :clamper-vy
     (clamper+- terminal-speed))))

(defmethod draw ((d dorito))
  (with-dorito-slots (d)
    (dorito-draw life-timer (timed-cycle-current anim-cycle) (dorito-size d) (dorito-pos d))))

(defmethod stage-collision ((d dorito) stage)
  (modify-dorito (d)
    (mvsetq (pos vel)
	    (dorito-stage-collisions pos vel size stage))))

(defmethod pickup-rect ((d dorito))
  (rect-offset (dorito-collision-rect (dorito-size d)) (dorito-pos d)))

(defmethod pickup-kill ((d dorito))
  (modify-dorito (d)
    (push-sound :pickup)
    (tf dead?)))

(defmethod pickup-data ((d dorito))
  (make-pickup :type :dorito :amt (ecase (dorito-size d)
				    (:small 1)
				    (:medium 10)
				    (:large 20))))

(defmethod dead? ((d dorito))
  (dorito-dead? d))

(defun create-dorito (pos vel size)
  (create-entity
   (make-dorito :pos pos
		:vel vel
		:size size)
   '(:ai :physics :stage-collision :drawable :pickup)))

(defun dorito-draw (life-tr anim-cycle-current size pos)
  (unless (and (< (timer-ms-remaining life-tr) (s->ms 1))
	       (zerop (chunk-timer-period life-tr 50)))
    (draw-sprite
     :pickup :npc-sym

     (create-rect
      (tile-v anim-cycle-current
	      (1+ (position size '(:small :medium :large))))
      (make-v (tiles 1) (1- (tiles 1))))

     pos)))

(defun dorito-collision-rect (size)
  (centered-rect (tile-dims/2)
		 (ecase size
		   (:small (both-v (tiles 3/5)))
		   (:medium (both-v (tiles 3/4)))
		   (:large (both-v (tiles 1))))))

(defun dorito-stage-collisions (pos vel size stage)
  (let ((vel (copy-v2 vel))
	(reverse-x (collision-lambda
		     (setf (x vel) (- (x vel))))))
    (stage-collisions (pos (rect->collision-rects (dorito-collision-rect size)) stage)
      :bottom
      (collision-lambda
	(setf (y vel) (- dorito-bounce-speed))
	(push-sound :dorito-bounce))

      :right reverse-x
      :left reverse-x

      :top
      (collision-lambda
	(maxf (y vel) 0)))
    (values pos vel)))

(defstructure particle
    dead?
  timed-cycle
  sheet-key
  tile-y
  pos)

(defmethod ai ((p particle))
  (modify-particle (p)
    (mvbind (tc ticked?) (update-timed-cycle timed-cycle)
      (setf timed-cycle tc)
      (when (and ticked?
		 (zerop (cycle-idx (timed-cycle-cycle timed-cycle))))
	(tf dead?)))))

(defmethod draw ((p particle))
  (with-particle-slots (p)
    (particle-draw sheet-key (timed-cycle-cycle timed-cycle) tile-y pos)))

(defmethod dead? ((p particle))
  (particle-dead? p))

(defun create-particle (&key seq fps sheet-key tile-y pos (id (gen-entity-id)))
  (create-entity
   (make-particle :timed-cycle (make-timed-cycle :timer (fps-make-timer fps)
						 :cycle (create-cycle seq))
		  :sheet-key sheet-key
		  :tile-y tile-y
		  :pos pos)
   '(:ai :drawable)
   :id id))

(defun particle-draw (sheet-key cycle tile-y pos)
  (draw-sprite :particle sheet-key
	       (tile-rect (tile-v (cycle-current cycle) tile-y))
	       pos))

(defun make-projectile-star-particle (center-pos)
  (create-particle :seq (alexandria:iota 4)
		   :fps 14
		   :sheet-key :caret
		   :tile-y 3
		   :pos (sub-v center-pos (tile-dims/2))))

(defun make-projectile-wall-particle (center-pos)
  (create-particle :seq (mapcar (curry #'+ 11) (alexandria:iota 4))
		   :fps 14
		   :sheet-key :caret
		   :tile-y 0
		   :pos (sub-v center-pos (tile-dims/2))))

(defun draw-number (pos number &key (push-fn #'push-render) (layer :hud) (centered? t) (show-sign? t))
  (let* ((neg? (minusp number))
	 (digits (fixnum->digits number))
	 (pos (if centered?
		  (sub-v pos (tiles/2-v (/ (length digits) 2) 1/2))
		  pos)))

    (dolist (digit digits)
      (when (or show-sign? (numberp digit))
	(let ((src-pos
	       (case digit
		 (:positive (tiles/2-v 4 6))
		 (:negative (tiles/2-v 5 6))
		 (t (tiles/2-v digit (if neg? 8 7))))))
	  (funcall push-fn (make-sprite-drawing
			    :layer layer
			    :sheet-key :text-box
			    :src-rect (create-rect src-pos (tile-dims/2))
			    :pos pos)))
	(+vf pos (tiles/2-v 1 0))))))

(defun fixnum->digits (number)
  (if (zerop number)
      (list 0)
      (let (digits
	    neg?)
	(when (minusp number)
	  (tf neg?)
	  (setf number (* -1 number)))
	(while (plusp number)
	  (mvbind (num rem) (floor number 10)
	    (push rem digits)
	    (setf number num)))
	(if neg?
	    (cons :negative digits)
	    (cons :positive digits)))))

(defstructure floating-number
    (offset (make-offset-motion
	     :dir :up
	     :speed
	     (/ (tiles 1/30) frame-time)
	     :max-dist (tiles 1)))

  (life-timer (let ((time (s->ms 2)))
		(create-expiring-timer time t)))
  entity
  amt)

(defmethod draw ((fn floating-number))
  (draw-number (+v (floating-number-origin (estate (floating-number-entity fn)))
		   (offset-motion-offset (floating-number-offset fn)))
	       (floating-number-amt fn)))

(defmethod ai ((dn floating-number))
  (modify-floating-number (dn)
    (fnf life-timer #'update-timer)))

(defmethod dead? ((dn floating-number))
  (floating-number-dead? dn))

(defun floating-number-add-amt (dn amount)
  (modify-floating-number (dn)
    (fnf life-timer #'reset-timer)
    (incf amt amount)))

(defun create-floating-number (entity amt &key (id (gen-entity-id)))
  (create-entity
   (make-floating-number :entity entity :amt amt)
   '(:ai :drawable :physics)
   :id id))

(defmethod physics ((fn floating-number))
  (modify-floating-number (fn)
    (fnf offset #'offset-motion-physics)))

(defun floating-number-dead? (fn)
  (not (timer-active? (floating-number-life-timer fn))))

(defun remove-all-dead (game)
  (replace-entity-state (game-projectile-groups game) #'projectile-groups-remove-dead)
  (replace-entity-state (game-damage-numbers game) #'damage-numbers-remove-dead))

(defun draw-bat (pos facing cycle-idx)
  (draw-sprite :enemy :npc-cemet
	       (tile-rect (tile-v (+ 2 cycle-idx)
				  (if (eq facing :left) 2 3)))
	       pos))

(defun draw-hud-number (tile/2-y number)
  (draw-number (tiles/2-v (+ (if (< number 10) 1 0) 3)
			  tile/2-y)
	       number
	       :centered? nil
	       :show-sign? nil
	       :push-fn #'push-screen-render))

(defun exp-for-gun-level (gun-name lvl)
  (when (eq lvl :max)
    (setf lvl 2))
  (elt (cdr (assoc gun-name gun-level-exps)) lvl))

(comment-code
  ("If there are no more red flowers we can, hopefully, avoid the war."
   :wait
   :clear-text
   "Well, that's a pretty heavy responsibility, you think?"
   :wait
   :close)
  ("Do you want to save?"
   (:yes-or-no
    yes-fn
    no-fn))
  ("Aaaaahhh!"
   :wait
   :close
   :empty-text
   (:open :curly)
   "Hey!!"
   :wait
   :close
   (:pause 50)
   :open
   "Look, a visitor after such a long time!!"
   :wait
   "I know what you want to do!"
   :wait
   "But you better wake up!" ;; Appends text.
   :wait
   "The Mimiga aren't the enemy!"
   :wait
   "They're totally harmless!"
   :wait
   :clear-text
   "I feel sorry for you..."
   :wait
   :close
   ;; Other stuff...
   :open
   "I'm on the Mimiga side and not gonna lose to you!!"
   :wait))

(defun slow-text-speed ()
  (setf text-speed 100))
(defun fast-text-speed ()
  (setf text-speed 25))
(defparameter text-speed 100)
(defparameter cursor-blink-time 100)

(defstructure text-display
    pos
  text
  (num-chars 0)
  (timer (create-expiring-timer text-speed t))
  (wait-for-input? t)
  (blink-time 0)
  dead?)

(defmethod ai ((td text-display))
  (modify-text-display (td)
    (fnf timer #'update-timer)
    (cond
      ((= num-chars (length text))
       (incf blink-time frame-time)
       (when (and wait-for-input?
		  (> 2 (chunk-time-period blink-time cursor-blink-time 5)))
	 (let ((char-dims (get-text-size font " "))
	       (w (x (get-text-size font text))))
	   (push-screen-render
	    (make-rect-drawing :rect (create-rect (+v pos (make-v w 0)) char-dims)
			       :color white
			       :layer :text
			       :filled? t)))))
      ((not (timer-active? timer))
       (incf num-chars)
       (push-sound :text-click)
       (setf (timer-length timer) text-speed)
       (fnf timer #'reset-timer)))))

(defmethod draw ((td text-display))
  (with-text-display-slots (td)
    (draw-textbox 5 21 30 8)
    (draw-text-line pos (subseq text 0 num-chars))))

(defmethod dead? ((td text-display))
  (text-display-dead? td))

(defun create-text-display (pos text)
  (let ((entity-system-type :dialog))
    (create-entity (make-text-display :pos pos :text text)
		   '(:ai :drawable))))

(defstructure hud
    player
  gun-exps

  last-health-amt
  (timer (create-expiring-timer (s->ms 1)))
  (health-change-timer (create-expiring-timer (s->ms 1/2))))

(defmethod draw ((h hud))
  (with-hud-slots (h)
    (let ((bar-tile/2-w 5)
	  (bar-tile/2-x 5))
      (draw-hud-sprite
       :hud-bg :text-box
       (create-rect-cmpts 0 (tiles/2 5) (tiles/2 8) (tiles/2 1))
       (tile-v 1 2))

      (let ((health (player-health-amt (player-state player))))
	(when (timer-active? health-change-timer)
	  (draw-hud-sprite
	   :hud :text-box
	   (create-rect-cmpts 0 (tiles/2 4)
			      (floor (* (tiles/2 bar-tile/2-w)
					(/ last-health-amt
					   (player-max-health-amt (player-state player)))))
			      (tiles/2 1))
	   (tiles/2-v bar-tile/2-x 4)))
	(draw-hud-sprite
	 :hud-fg :text-box
	 (create-rect-cmpts 0 (tiles/2 3)
			    (floor (* (tiles/2 bar-tile/2-w)
				      (/ health
					 (player-max-health-amt (player-state player)))))
			    (tiles/2 1))
	 (tiles/2-v bar-tile/2-x 4))
	(draw-hud-number 4 health))

      (let ((exp-pos (tiles/2-v bar-tile/2-x 3)))
	(draw-hud-sprite
	 :hud-bg :text-box
	 (create-rect-cmpts 0 (tiles/2 9) (tiles/2 5) (tiles/2 1))
	 exp-pos)

	(when (and (timer-active? timer)
		   (zerop (chunk-timer-period timer 50)))
	  (draw-hud-sprite :hud-fg
			   :text-box
			   (create-rect (tiles/2-v 5 10)
					(tiles/2-v bar-tile/2-w 1))
			   exp-pos))

	(mvbind (exp gun-name) (current-gun-exp player gun-exps)
	  (let* ((current-level (gun-level exp (cdr (assoc gun-name gun-level-exps))))
		 (next-lvl-exp (exp-for-gun-level gun-name current-level))
		 (current-lvl-exp (if (zerop current-level) 0 (exp-for-gun-level gun-name (1- current-level)))))
	    (if (= exp (exp-for-gun-level gun-name :max))
		(draw-hud-sprite :hud-fg :text-box
				 (create-rect (tiles/2-v 5 9)
					      (tiles/2-v bar-tile/2-w 1))
				 exp-pos)

		(draw-hud-sprite
		 :hud :text-box
		 (create-rect-cmpts 0 (tiles/2 10)
				    (floor (* (tiles/2 bar-tile/2-w)
					      (/ (- exp current-lvl-exp)
						 (- next-lvl-exp current-lvl-exp))))
				    (tiles/2 1))
		 exp-pos))

	    (draw-hud-sprite
	     :hud :text-box
	     (create-rect-cmpts (tiles/2 10) (tiles/2 10)
				(tiles/2 2) (tiles/2 1))
	     (make-v (tiles/2 2) (y exp-pos)))

	    (draw-hud-number 3 (1+ current-level))))))))

(defmethod ai ((h hud))
  (modify-hud (h)
    (fnf timer #'update-timer)
    (fnf health-change-timer #'update-timer)))

(defmethod dead? ((h hud))
  (dead? (estate (hud-player h))))

(defun hud-exp-changed (h)
  (modify-hud (h)
    (fnf timer #'reset-timer)))
(defun hud-health-changed (h)
  (modify-hud (h)
    (setf last-health-amt (player-health-amt (player-state player)))
    (fnf health-change-timer #'reset-timer)))

(defun create-hud (player gun-exps &key (id (gen-entity-id)))
  (create-entity
   (make-hud :player player :gun-exps gun-exps)
   '(:ai :drawable)
   :id id))

(defun draw-textbox-tile (src-pos pos)
  (let ((size (both-v (tiles/2 1))))
    (draw-hud-sprite :text-box :text-box
		     (create-rect src-pos size)
		     pos)))

(defun draw-textbox (text-x text-y text-width text-height)
  (let ((right 30)
	(left 0)
	(top 0)
	(mid 1)
	(bottom 2)
	(top-y text-y)
	(bottom-y (+ text-y text-height -1)))

    (draw-textbox-tile (tiles/2-v left top) (tiles/2-v text-x top-y))
    (draw-textbox-tile (tiles/2-v left bottom) (tiles/2-v text-x bottom-y))

    (dotimes (i (- text-height 2))
      (draw-textbox-tile (tiles/2-v left mid) (tiles/2-v text-x (+ i text-y 1))))

    (dotimes (i (- text-width 2))
      (let ((x (+ 1 i text-x)))
	(dotimes (j (- text-height 2))
	  (draw-textbox-tile (tiles/2-v mid mid) (tiles/2-v x (+ j text-y 1))))
	(draw-textbox-tile (tiles/2-v mid top) (tiles/2-v x top-y))
	(draw-textbox-tile (tiles/2-v mid bottom) (tiles/2-v x bottom-y))))

    (let ((x (+ -1 text-width text-x)))
      (draw-textbox-tile (tiles/2-v right top) (tiles/2-v x top-y))
      (dotimes (i (- text-height 2))
	(draw-textbox-tile (tiles/2-v right mid) (tiles/2-v x (+ 1 text-y i))))
      (draw-textbox-tile (tiles/2-v right bottom) (tiles/2-v x bottom-y)))))

(defun update-and-render (renderer game)
  "The Main Loop, called once per FRAME-TIME."
  (nilf render-list screen-render-list)

  (let ((active-update-systems (active-systems-update (estate (game-active-systems game))))
	(stage (game-stage game))
	(player (game-player game)))
    (update-ai-subsystem active-update-systems)
    (update-physics-subsystem active-update-systems)
    (update-bullet-subsystem active-update-systems)
    (update-stage-collision-subsystem active-update-systems stage)
    (update-pickup-subsystem active-update-systems player)

    (update-damage-collision-subsystem active-update-systems player)
    (update-dynamic-collision-subsystem active-update-systems player))

  (let ((active-draw-systems (active-systems-draw (estate (game-active-systems game)))))
    (update-drawable-subsystem active-draw-systems))

  (remove-all-dead game)

  ;; Debug Drawings Below.

  ;; (draw-point (player-nozzle-pos player) red)
  (let ((focus (camera-focus (estate (game-camera game))))
	(camera-bounds (stage-dims->camera-bounds (stage-dims (game-stage game)))))
    (draw-point focus cyan)
    (draw-point (clamp-pos focus camera-bounds) red)
    (draw-rect camera-bounds cyan))
  (draw-point (camera-target-from-player (player-state (game-player game))) white)
  ;; End Debug Drawings.

  (play-sounds sfx-play-list)
  (nilf sfx-play-list))

(defparameter dialog-text-pos (tiles/2-v 7 24))

(defvar global-paused?)

(defparameter entity-systems '(:game :dialog))

(defstructure active-systems
    (update (list :game))
  (draw (list :game))
  (input (list :game)))

(defun active-systems-switch-to-dialog (a)
  (modify-active-systems (a)
    (setf update (list :dialog))
    (setf draw (list :game :dialog))
    (setf input (list :dialog))))

(defun create-active-systems (&key (id (gen-entity-id)))
  (create-entity (make-active-systems) () :id id))

(defun damage-numbers-remove-dead (d)
  (remove-if (lambda (pair) (dead? (estate (cdr pair)))) d))

(defun damage-numbers-update-damage-amt (d e amt)
  (aif (assoc e d)
       (replace-entity-state (cdr it) (lambda (fn) (floating-number-add-amt fn (- amt))))
       (push (cons e (create-floating-number e (- amt))) d))
  d)

(defun create-damage-numbers (&key (id (gen-entity-id)))
  (create-entity nil () :id id))

(defun gun-exp-for (gun-exps gun-name)
  (cdr (assoc gun-name gun-exps)))

(defun incr-gun-exp (gun-exps gun-name amt)
  (let ((g (copy-alist gun-exps)))
    (let ((pair (assoc gun-name g)))
      (setf (cdr pair) (min (max (+ (cdr pair) amt) 0)
			    (exp-for-gun-level gun-name :max))))
    g))

(defun create-gun-exps (&key (id (gen-entity-id)))
  (create-entity
   (loop for g across gun-names collecting (cons g 0))
   ()
   :id id))

(defun projectile-groups-remove-dead (g)
  (remove-if #'null
	     (loop for (name . g) in g
		collecting
		  (let ((new-g (remove-if (lambda (x) (dead? (estate x))) g)))
		    (when new-g
		      (list* name new-g))))))

(defun projectile-groups-count (g gun-name)
  (count gun-name g :key #'car))

(defun projectile-groups-add (g pg)
  (push pg g))

(defun create-projectile-groups (&key (id (gen-entity-id)))
  (create-entity nil () :id id))

(defun update-damage-number-amt (damage-numbers e amt)
  (replace-entity-state damage-numbers (lambda (dn) (damage-numbers-update-damage-amt dn e amt))))

(defun current-gun-exp (player gun-exps)
  (let* ((gun-name (player-current-gun-name (player-gun-name-cycle (player-state player))))
	 (exp (gun-exp-for (estate gun-exps) gun-name)))
    (values
     exp
     gun-name)))

(defun save-current-state ()
  (cl-store:store (current-entity-states) "./temp.entities")
  (cl-store:store global-game "./temp.game"))

(defun restore-state ()
  (dolist (s registry-syms)
    (nilf (symbol-value s)))
  (restore-entity-states (cl-store:restore "./temp.entities"))
  (setf global-game (cl-store:restore "./temp.game")))

(defun create-game ()
  (let ((damage-numbers (create-damage-numbers))
	(projectile-groups (create-projectile-groups))
	(stage (basic-stage))
	(hud (gen-entity-id))
	(gun-exps (create-gun-exps))
	(active-systems (create-active-systems)))
    (let* ((player (create-default-player hud projectile-groups damage-numbers gun-exps active-systems))
	   (camera (create-player-camera (v/2 window-dims) (zero-v) player)))
      (create-hud player gun-exps :id hud)

      (create-critter (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) player damage-numbers)
      (dolist (x '(1 3 6 7))
	(create-bat x 7 player))
      (create-dorito (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) (make-v 0 0) :medium)
      (make-game :player player
		 :camera camera
		 :stage stage
		 :projectile-groups projectile-groups
		 :active-systems active-systems
		 :damage-numbers damage-numbers))))

(defun reset ()
  (switch-to-new-song :lastcave)
  (set-music-volume 20)

  (dolist (s registry-syms)
    (nilf (symbol-value s)))
  (init-entity-registry)

  (nilf global-paused?)

  (create-game))

(defun init ()
  "Called at application startup."
  (sdl:init '(:audio :video :joystick))
  (sdl.ttf:init)
  (setf font (sdl.ttf:open-font "./content/VeraMoBd.ttf" 19))
  (sdl.mixer:open-audio sdl.mixer:+default-frequency+
			sdl.mixer:+default-format+
			2
			4096)

  (init-input)
  (sdl:show-cursor :disable)

  (put-all-resources)
  (nilf current-song)

  (mvsetq
   (window renderer)
   (sdl:default-window-and-renderer
       "Cave Story"
       (x window-dims) (y window-dims)
       0))
  (reset))

(defun cleanup ()
  "Called at application closing to cleanup all subsystems."
  (cleanup-input)
  (cleanup-all-resources)
  (clrhash character-textures)

  (sdl.mixer:close-audio)
  (sdl:destroy-renderer renderer)
  (sdl:destroy-window window)
  (sdl.ttf:quit)
  (sdl:quit)
  (nilf renderer window))

(defun quit ()
  "Quits the application."
  (throw 'exit nil))

(defun face-player (pos player)
  (if (< (x pos) (x (player-pos (player-state player)))) :right :left))

(defstructure bat
    player

  origin
  facing
  wave-motion
  anim-cycle
  dead?)

(defmethod ai ((b bat))
  (modify-bat (b)
    (fnf anim-cycle #'update-timed-cycle)
    (setf facing (face-player (bat-pos wave-motion origin) player))))

(defmethod physics ((b bat))
  (modify-bat (b)
    (fnf wave-motion #'wave-physics)))

(defmethod draw ((b bat))
  (with-bat-slots (b)
    (draw-bat (bat-pos wave-motion origin) facing (timed-cycle-current anim-cycle))))

(defmethod damage-collision-rect ((b bat))
  (with-bat-slots (b)
    (bat-damage-collision-rect (bat-pos wave-motion origin))))
(defmethod damage-collision-amt ((b bat))
  1)

(defmethod damageable-rect ((b bat))
  (with-bat-slots (b)
    (bat-collision-rect (bat-pos wave-motion origin))))
(defmethod damageable-hit-react ((b bat) bullet-hit-amt)
  (declare (ignore bullet-hit-amt))
  (modify-bat (b)
    (push-sound :enemy-explode)
    (create-dorito (bat-pos wave-motion origin) (zero-v) :large)
    (tf dead?)))

(defmethod dead? ((b bat)) (bat-dead? b))

(defun create-bat (tile-x tile-y player)
  (create-entity
   (make-bat :origin (tile-v tile-x tile-y)
	     :wave-motion (make-wave-motion :dir :up
					    :amp (tiles 2)
					    :speed (/ 0.0325 frame-time))
	     :anim-cycle (create-timed-cycle 14 #(0 2 1 2))
	     :player player)
   '(:ai :damage-collision :damageable :drawable :physics)))

(defun bat-pos (wave-motion origin)
  (+v (wave-offset wave-motion) origin))

(defun bat-damage-collision-rect (pos)
  (create-rect (+v pos (tile-dims/2))
	       (both-v 1)))

(defun bat-collision-rect (pos)
  (create-rect pos (tile-dims)))

(defparameter critter-dynamic-collision-rect
  (make-rect :pos (tile-v 0 1/4) :size (tile-v 1 3/4)))

(defstructure critter
    id
  pos
  dead?
  vel
  ground-tile
  facing
  sleep-timer

  player
  damage-numbers)

(defmethod ai ((c critter))
  (modify-critter (c)
    (fnf sleep-timer #'update-timer)
    (setf facing (face-player pos player))
    (let ((disp (- (x pos) (x (player-pos (player-state player))))))
      (when (and ground-tile
		 (not (timer-active? sleep-timer))
		 (< (abs disp) (tiles 2)))
	(setf (x vel) (* 0.04 (- (signum disp))))
	(setf (y vel) -0.35)))))

(defmethod physics ((c critter))
  (modify-critter (c)
    (physics-2d pos vel
		(const-accelerator 0)
		(const-accelerator gravity-acc)
		:clamper-vy
		(clamper+- terminal-speed))))

(defmethod draw ((c critter))
  (with-critter-slots (c)
    (let ((sprite-tile-x (if (< (abs (- (x pos) (x (player-pos (player-state player))))) (tiles 4))
			     1
			     0)))
      (draw-sprite :enemy :npc-cemet (tile-rect (tile-v sprite-tile-x (if (eq facing :left) 0 1))) pos))))

(defmethod damageable-rect ((c critter))
  (tile-rect (critter-pos c)))

(defmethod damageable-hit-react ((c critter) amt)
  (update-damage-number-amt (critter-damage-numbers c) (critter-id c) amt)
  (push-sound :enemy-hurt)
  c)

(defmethod damage-collision-rect ((c critter))
  (tile-rect (critter-pos c)))

(defmethod damage-collision-amt ((c critter))
  1)

(defmethod dynamic-collision-rect ((c critter))
  (rect-offset critter-dynamic-collision-rect (critter-pos c)))

(defmethod dynamic-collision-vel ((c critter))
  (critter-vel c))

(defmethod dynamic-collision-react ((c critter) side player-collision-rect player)
  (let ((new-p (player-state player)))
    (with-critter-slots (c)
      (modify-player (new-p p-)
	(let ((player-rect (rect-offset player-collision-rect p-pos)))
	  (case side
	    (:bottom
	     (when (and (not (player-on-ground? p-ground-tile))
			(<= (y pos) (bottom player-rect) (+ (y pos) (tiles 1/2))))
	       (setf p-ground-tile :dynamic)
	       (setf p-ground-inertia-entity id)
	       (setf p-vel (zero-v :x (x p-vel)))

	       (setf p-pos
		     (-v
		      (flush-rect-pos player-rect (y (+v (rect-pos critter-dynamic-collision-rect) pos)) :up)
		      (rect-pos player-collision-rect)))))
	    ((:left :right)
	     (let ((disp (- (x p-pos) (x pos))))
	       (when (> (abs disp) (tiles 1/4))
		 (setf (x p-vel)
		       (* (/ terminal-speed 70) disp)))))))))))

(let ((collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6)))
  (defmethod stage-collision ((c critter) stage)
    (modify-critter (c)
      (let (new-tile)
	(stage-collisions (pos collision-rects stage)
	  :bottom
	  (collision-lambda
	    (setf new-tile tile-type)
	    (unless ground-tile
	      (fnf sleep-timer #'reset-timer))
	    (allf 0 (y vel) (x vel)))

	  :right (collision-lambda)
	  :left (collision-lambda)

	  :top
	  (collision-lambda
	    (maxf (y vel) 0)))
	(setf ground-tile new-tile)))))

(defmethod dead? ((c critter))
  (critter-dead? c))

(defgeneric floating-number-origin (obj))
(defmethod floating-number-origin ((c critter))
  (+v (critter-pos c) (tile-dims/2)))

(defgeneric inertia-vel (obj))
(defmethod inertia-vel ((c critter))
  (critter-vel c))

(defun create-critter (pos player damage-numbers &key (id (gen-entity-id)))
  (create-entity
   (make-critter :pos pos
		 :player player
		 :damage-numbers damage-numbers
		 :vel (zero-v)
		 :sleep-timer (create-expiring-timer 300)
		 :id id)
   '(:ai :drawable :physics :damageable :damage-collision :dynamic-collision :stage-collision)
   :id id))
