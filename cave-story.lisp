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

(defun main ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       last-update-time
	       game)
	   (setq game (init))
	   (setf last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (setf game (handle-input game))
		  (let ((input (game-input game)))
		    (when (and global-paused? (key-pressed? input :n))
		      (update-and-render renderer game))
		    (when (or (key-pressed? input :r) (joy-pressed? input :select))
		      (setq game (reset))))
		  (when (>= frame-timer (* update-period frame-time))
		    (unless global-paused?
		      (update-and-render renderer game))
		    (render renderer
			    font
			    render-list
			    (camera-focus->camera-pos
			     (clamp-pos (ecall (game-camera game) :focus)
					(stage-dims->camera-bounds (stage-dims (game-stage game))))))
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


    (update-input-subsystem (ecall (game-active-systems game) :input) input)
    (ecase (first (ecall (game-active-systems game) :input))
      (:game
       (when (or (joy-pressed? input :b) (key-pressed? input :x))
	 ;; Fire Gun
	 (player-fire-gun (game-player game))))

      (:dialog
       (cond
	 ((or (joy-pressed? input :b) (key-pressed? input :x))
	  (dialog-ok-pressed))
	 ((or (joy-held? input :a) (key-pressed? input :z)
	      (joy-held? input :b) (key-pressed? input :x))
	  (dialog-button-held))
	 (t
	  (dialog-buttons-released)))))
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

(defun create-dorito (pos vel size)
  (let* ((d (make-dorito :pos pos
			 :vel vel
			 :size size))
	 (dead?-fn (lambda () (dorito-dead? d))))

    (def-entity-timer
	(() (setf d (with-dorito-copy-slots (d)
		      (fnf anim-cycle #'update-timed-cycle)
		      d))))

    (def-entity-ai
	(()
	 (setf d (with-dorito-copy-slots (d)
		   (fnf life-timer #'update-timer)
		   (unless (timer-active? life-timer)
		     (tf dead?))
		   d))))

    (def-entity-physics
	(()
	 (setf d (with-dorito-copy-slots (d)
		   (physics-2d
		    pos
		    vel
		    (friction-accelerator dorito-friction-acc)
		    (const-accelerator gravity-acc)
		    :clamper-vy
		    (clamper+- terminal-speed))
		   d))))

    (def-entity-stage-collision
	((stage)
	 (setf d (with-dorito-copy-slots (d)
		   (mvsetq (pos vel)
			   (dorito-stage-collisions pos vel size stage))
		   d))))

    (def-entity-drawable
	(()
	 (with-dorito-slots (d)
	   (dorito-draw life-timer (timed-cycle-current anim-cycle) (dorito-size d) (dorito-pos d)))))

    (def-entity-pickup
	(()
	 (rect-offset (dorito-collision-rect size) (dorito-pos d)))
	(()
	 (setf d
	       (with-dorito-copy-slots (d)
		 (push-sound :pickup)
		 (tf dead?)
		 d)))
      (()
       (make-pickup :type :dorito :amt (ecase (dorito-size d)
					 (:small 1)
					 (:medium 10)
					 (:large 20)))))))

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

(defun create-particle (&key seq fps sheet-key tile-y pos (id (gen-entity-id)))
  (let* ((p (make-particle :timed-cycle (make-timed-cycle :timer (fps-make-timer fps)
							  :cycle (create-cycle seq))
			   :sheet-key sheet-key
			   :tile-y tile-y
			   :pos pos))
	 (dead?-fn (lambda () (particle-dead? p))))

    (def-entity-timer
	(()
	 (modify-particle (p)
	   (mvbind (tc ticked?) (update-timed-cycle timed-cycle)
	     (setf timed-cycle tc)
	     (when (and ticked?
			(zerop (cycle-idx (timed-cycle-cycle timed-cycle))))
	       (tf dead?))))))

    (def-entity-drawable
	(()
	 (with-particle-slots (p)
	   (particle-draw sheet-key (timed-cycle-cycle timed-cycle) tile-y pos)))))
  (register-entity-interface
   id
   (dlambda)))

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

(defun draw-floating-number (fn)
  (draw-number (+v (ecall (floating-number-entity fn) :origin)
		   (offset-motion-offset (floating-number-offset fn)))
	       (floating-number-amt fn)))

(defun create-floating-number (entity amt &key (id (gen-entity-id)))
  (let* ((dn (make-floating-number :entity entity :amt amt))
	 (dead?-fn (lambda () (floating-number-dead? dn))))

    (def-entity-timer
	(()
	 (modify-floating-number (dn)
	   (fnf life-timer #'update-timer))))

    (def-entity-drawable
	(() (draw-floating-number dn)))

    (def-entity-physics
	(()
	 (fnf dn #'floating-number-physics)))

    (register-entity-interface
     id
     (dlambda
      (:dead? () (funcall dead?-fn))
      (:add-amt (amount) (modify-floating-number (dn)
			   (fnf life-timer #'reset-timer)
			   (incf amt amount)))))))

(defun floating-number-physics (fn)
  (modify-floating-number (fn)
    (fnf offset #'offset-motion-physics)))

(defun floating-number-dead? (fn)
  (not (timer-active? (floating-number-life-timer fn))))

(defun remove-all-dead (game)
  (ecall (game-projectile-groups game) :remove-dead)
  (ecall (game-damage-numbers game) :remove-dead))

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

(defun create-text-display (pos text)
  (let* ((entity-system-type :dialog)
	 (td (make-text-display :pos pos :text text))
	 (dead?-fn (lambda () (text-display-dead? td))))

    (def-entity-timer
	(()
	 (setf td (with-text-display-copy-slots (td)
		    (fnf timer #'update-timer)
		    td))))

    (def-entity-ai
	(()
	 (setf td (with-text-display-copy-slots (td)
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
		       (fnf timer #'reset-timer)))
		    td))))

    (def-entity-drawable
	(()
	 (with-text-display-slots (td)
	   (draw-textbox 5 21 30 8)
	   (draw-text-line pos (subseq text 0 num-chars)))))))

(defun create-jenka (player)
  (let* ((dead?)
	 facing
	 (pos (tile-v 24 9))
	 (dead?-fn (lambda () dead?))
	 (tc (create-timed-cycle 3/2 #(0 1))))

    (def-entity-timer (() (fnf tc #'update-timed-cycle)))
    (def-entity-ai
	(()
	 (setf facing (face-player pos player))))
    (def-entity-drawable
	(()
	 (draw-sprite :npc
		      :npc-regu
		      (tile-rect (tile-v (+ 11 (timed-cycle-current tc))
					 (if (eq facing :left) 2 3)))
		      pos)))))

(defstructure hud
    player
  gun-exps

  last-health-amt
  (timer (create-expiring-timer (s->ms 1)))
  (health-change-timer (create-expiring-timer (s->ms 1/2))))

(defun create-hud (player gun-exps &key (id (gen-entity-id)))
  ;; Player and current-gun-exp-fn are references to outside entities.
  (let* (;; State-vars
	 (h (make-hud :player player :gun-exps gun-exps))
	 (dead?-fn (lambda () (player-dead? player))))

    (def-entity-timer
	(()
	 (setf h (with-hud-copy-slots (h)
		   (fnf timer #'update-timer)
		   (fnf health-change-timer #'update-timer)
		   h))))

    (def-entity-drawable
	(()
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

		   (draw-hud-number 3 (1+ current-level)))))))))

    (register-entity-interface
     id
     (dlambda
      (:exp-changed () (setf h (with-hud-copy-slots (h)
				 (fnf timer #'reset-timer)
				 h)))
      (:health-changed ()
		       (setf h (with-hud-copy-slots (h)
				 (setf last-health-amt (player-health-amt (player-state player)))
				 (fnf health-change-timer #'reset-timer)
				 h)))))))

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

  (let ((active-update-systems (ecall (game-active-systems game) :update))
	(stage (game-stage game))
	(player (game-player game)))
    (update-ai-subsystem active-update-systems)
    (update-physics-subsystem active-update-systems)
    (update-bullet-subsystem active-update-systems)
    (update-stage-collision-subsystem active-update-systems stage)
    (update-pickup-subsystem active-update-systems player)

    (update-damage-collision-subsystem active-update-systems player)
    (update-dynamic-collision-subsystem active-update-systems player))

  (let ((active-draw-systems (ecall (game-active-systems game) :draw)))
    (update-drawable-subsystem active-draw-systems))

  (remove-all-dead game)

  ;; Debug Drawings Below.

  ;; (draw-point (player-nozzle-pos player) red)
  (let ((focus (ecall (game-camera game) :focus))
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

(defstructure game
    player
  camera
  stage
  projectile-groups
  damage-numbers
  active-systems

  (input (make-input)))

(defparameter entity-systems '(:game :dialog))

(defstructure active-systems
    (update (list :game))
  (draw (list :game))
  (input (list :game)))

(defun create-active-systems (&key (id (gen-entity-id)))
  (let ((a (make-active-systems)))
    (register-entity-interface
     id
     (dlambda
      (:input () (active-systems-input a))
      (:update () (active-systems-update a))
      (:draw () (active-systems-draw a))
      (:switch-to-dialog ()
			 (modify-active-systems (a)
			   (setf update (list :dialog))
			   (setf draw (list :game :dialog))
			   (setf input (list :dialog))))))))

(defun create-damage-numbers (&key (id (gen-entity-id)))
  (let ((d nil))
    (register-entity-interface
     id
     (dlambda
      (:remove-dead ()
		    (setf d (remove-if (lambda (pair) (ecall (cdr pair) :dead?)) d)))
      (:update-damage-amt (e amt)
			  (aif (assoc e d)
			       (ecall (cdr it) :add-amt (- amt))
			       (push (cons e (create-floating-number e (- amt))) d)))))))

(defun create-gun-exps (&key (id (gen-entity-id)))
  (let ((g (loop for g across gun-names collecting (cons g 0))))
    (register-entity-interface
     id
     (dlambda
      (:exp-for (gun-name)
		(cdr (assoc gun-name g)))
      (:incr (gun-name amt)
	     (setf g (copy-alist g))
	     (let ((pair (assoc gun-name g)))
	       (setf (cdr pair) (min (max (+ (cdr pair) amt) 0)
				     (exp-for-gun-level gun-name :max)))))

      (t () (error "Bad Call."))))))

(defun create-projectile-groups (&key (id (gen-entity-id)))
  (let ((g nil))
    (register-entity-interface
     id
     (dlambda
      (:remove-dead ()
		    (setf g
			  (remove-if #'null
				     (loop for (name . g) in g
					collecting
					  (let ((new-g (remove-if (lambda (x) (ecall x :dead?)) g)))
					    (when new-g
					      (list* name new-g)))))))
      (:count (gun-name)
	      (count gun-name g :key #'car))
      (:add (pg)
	    (push pg g))))))

(defun update-damage-number-amt (damage-numbers e amt)
  (ecall damage-numbers :update-damage-amt e amt))

(defun current-gun-exp (player gun-exps)
  (let* ((gun-name (player-current-gun-name (player-gun-name-cycle (player-state player))))
	 (exp (ecall gun-exps :exp-for gun-name)))
    (values
     exp
     gun-name)))

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

      (create-jenka player)

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

  (init-entity-interface-registry)
  (dolist (s registry-syms)
    (nilf (symbol-value s)))

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
    origin
  facing
  wave-motion
  anim-cycle
  dead?)

(defun create-bat (tile-x tile-y player)
  (let* ((b (make-bat :origin (tile-v tile-x tile-y)
		      :wave-motion (make-wave-motion :dir :up
						     :amp (tiles 2)
						     :speed (/ 0.0325 frame-time))
		      :anim-cycle (create-timed-cycle 14 #(0 2 1 2))))
	 (dead?-fn (lambda () (bat-dead? b))))

    (def-entity-timer
	(()
	 (modify-bat (b)
	   (fnf anim-cycle #'update-timed-cycle))))
    (def-entity-ai
	(()
	 (modify-bat (b)
	   (setf facing (face-player (bat-pos wave-motion origin) player)))))
    (def-entity-damage-collision
	(()
	 (with-bat-slots (b)
	   (bat-damage-collision-rect (bat-pos wave-motion origin))))
	(() 1))

    (def-entity-damageable
	(()
	 (with-bat-slots (b)
	   (bat-collision-rect (bat-pos wave-motion origin))))
	((bullet-hit-amt)
	 (declare (ignore bullet-hit-amt))
	 (modify-bat (b)
	   (push-sound :enemy-explode)
	   (create-dorito (bat-pos wave-motion origin) (zero-v) :large)
	   (tf dead?))))

    (def-entity-drawable
	(()
	 (with-bat-slots (b)
	   (draw-bat (bat-pos wave-motion origin) facing (timed-cycle-current anim-cycle)))))

    (def-entity-physics
	(()
	 (modify-bat (b)
	   (fnf wave-motion #'wave-physics))))))

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
    pos
  dead?
  vel
  ground-tile
  facing
  sleep-timer

  player
  damage-numbers)

(let ((collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6)))
  (defun create-critter (pos player damage-numbers &key (id (gen-entity-id)))
    ;; Pos: State-var
    ;; Player: referenced-entity
    (let* ((c (make-critter :pos pos :player player :damage-numbers damage-numbers :vel (zero-v) :sleep-timer (create-expiring-timer 300)))
	   ;; My-interface
	   (dead?-fn (lambda () (critter-dead? c))))

      (def-entity-timer
	  (()
	   (setf c (with-critter-copy-slots (c)
		     (fnf sleep-timer #'update-timer)
		     c))))

      (def-entity-ai
	  (()
	   (setf c (with-critter-copy-slots (c)
		     (setf facing (face-player pos player))
		     (let ((disp (- (x pos) (x (player-pos (player-state player))))))
		       (when (and ground-tile
				  (not (timer-active? sleep-timer))
				  (< (abs disp) (tiles 2)))
			 (setf (x vel) (* 0.04 (- (signum disp))))
			 (setf (y vel) -0.35)))
		     c))))

      (def-entity-drawable
	  (()
	   (with-critter-slots (c)
	     (let ((sprite-tile-x (if (< (abs (- (x pos) (x (player-pos (player-state player))))) (tiles 4))
				      1
				      0)))
	       (draw-sprite :enemy :npc-cemet (tile-rect (tile-v sprite-tile-x (if (eq facing :left) 0 1))) pos)))))

      (def-entity-physics
	  (()
	   (setf c (with-critter-copy-slots (c)
		     (physics-2d pos vel
				 (const-accelerator 0)
				 (const-accelerator gravity-acc)
				 :clamper-vy
				 (clamper+- terminal-speed))
		     c))))

      (def-entity-damageable
	  (() (tile-rect (critter-pos c)))
	  ((amt)
	   (update-damage-number-amt damage-numbers id amt)
	   (push-sound :enemy-hurt)))

      (def-entity-damage-collision
	  (() (tile-rect (critter-pos c)))
	  (() 1))

      (def-entity-dynamic-collision
	  (() (rect-offset critter-dynamic-collision-rect (critter-pos c)))
	  (() (critter-vel c))
	((side player-collision-rect player)
	 (let ((new-p (player-state player)))
	   (with-critter-slots (c)
	     (with-player-slots (new-p p-)
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
			      (* (/ terminal-speed 70) disp)))))))
	       new-p)))))

      (def-entity-stage-collision
	  ((stage)
	   (setf c (with-critter-copy-slots (c)
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
		       (setf ground-tile new-tile))
		     c))))
      (register-entity-interface
       id
       (dlambda
	(:origin () (+v (critter-pos c) (tile-dims/2)))
	(:vel () (critter-vel c)))))))
