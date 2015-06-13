;;;; cave-story.lisp

(in-package #:cave-story)

(comment-code
  (progn
    (ql:quickload :cave-story)
    (in-package :cave-story)
    (swank:set-default-directory "/home/chebert/Projects/lisp/cave-story")))

(defvar window)
(defvar renderer)
(defvar input)

(defvar stage)
(defvar player)
(defvar projectile-groups)

(defvar gun-exps)

;; Debug params.
(defparameter paused? nil)
(defparameter update-period 1
  "Number of frames per update. 1 for real-time.")

(defvar font)

(defun main ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       last-update-time)
	   (init)
	   (setf last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (handle-input input renderer stage player)
		  (when (or (key-pressed? input :r) (joy-pressed? input :select))
		    (reset))
		  (when (>= frame-timer (* update-period frame-time))
		    (unless paused?
		      (update-and-render renderer stage player))
		    (render renderer
			    font
			    render-list
			    (camera-focus->camera-pos
			     (clamp-pos (camera-focus camera) (stage-dims->camera-bounds (stage-dims stage)))))
		    (decf frame-timer (* update-period frame-time)))

		  (let ((dt (- (sdl:get-ticks) last-update-time)))
		    ;; NOTE: if we are paused beyond our control, Don't play catchup.
		    (incf frame-timer (min dt (* 2 frame-time))))
		  (setf last-update-time (sdl:get-ticks))
		  (music-update)
		  (sdl:delay 1))))
      (cleanup))))

(let (debug-toggle-off?)
  (defun handle-input (input renderer stage player)
    "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."
    (gather-input input)
    (when (any? (held-keys input :capslock :escape))
      (quit))
    (when (or (key-pressed? input :p) (member 7 (ti-pressed-joy-buttons (input-transient-input input))))
      (togglef paused?))

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

    (ecase active-input-system
      (:game
       (player-input player input)
       (when (or (joy-pressed? input :b) (key-pressed? input :x))
	 ;; Fire Gun
	 (player-fire-gun player gun-exps))

       (when (or (key-pressed? input :a) (joy-pressed? input :y))
	 (setf (player-gun-name-cycle player) (cycle-previous (player-gun-name-cycle player))))

       (when (or (key-pressed? input :s) (joy-pressed? input :x))
	 (setf (player-gun-name-cycle player) (cycle-next (player-gun-name-cycle player)))))

      (:dialog
       (cond
	 ((or (joy-pressed? input :b) (key-pressed? input :x))
	  (dialog-ok-pressed))
	 ((or (joy-held? input :a) (key-pressed? input :z)
	      (joy-held? input :b) (key-pressed? input :x))
	  (dialog-button-held))
	 (t
	  (dialog-buttons-released)))))

    (when (and paused? (key-pressed? input :n))
      (update-and-render renderer stage player))))

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

(defstructure dorito
    cycle
  pos
  vel
  size
  (life-timer (let ((time (s->ms 8)))
		(create-timer :length time :ms-remaining time)))
  dead?)

(defstructure pickup type amt)

(defun create-dorito (pos vel size)
  (let* ((d (make-dorito :pos pos
			 :size size
			 :vel vel))
	 (dead?-fn (curry #'dorito-dead? d)))
    (setf (dorito-cycle d) (create-anim-cycle :fps 14 :seq (alexandria:iota 6)
					      :dead?-fn dead?-fn))

    (def-entity-ai
	(()
	 (unless (timer-active? (dorito-life-timer d))
	   (tf (dorito-dead? d)))))
    (def-entity-physics
	(()
	 (physics-2d
	  (dorito-pos d)
	  (dorito-vel d)
	  (friction-accelerator dorito-friction-acc)
	  (const-accelerator gravity-acc)
	  :clamper-vy
	  (clamper+- terminal-speed))))

    (def-entity-stage-collision
	((stage)
	 (dorito-stage-collisions d stage)))

    (def-entity-drawable
	(()
	 (dorito-draw d)))

    (def-entity-pickup
	(()
	 (rect-offset (dorito-collision-rect d) (dorito-pos d)))
	(()
	 (push-sound :pickup)
	 (tf (dorito-dead? d)))
      (()
       (make-pickup :type :dorito :amt (ecase size
					 (:small 1)
					 (:medium 10)
					 (:large 20)))))
    d))

(defun dorito-draw (d)
  (let ((life-tr (dorito-life-timer d)))
    (unless (and (< (timer-ms-remaining life-tr) (s->ms 1))
		 (zerop (chunk-timer-period life-tr 50)))
      (draw-sprite
       :pickup :npc-sym

       (create-rect
	(tile-v (anim-cycle-current (dorito-cycle d))
		(1+ (position (dorito-size d) '(:small :medium :large))))
	(make-v (tiles 1) (1- (tiles 1))))

       (dorito-pos d)))))

(defun dorito-collision-rect (d)
  (centered-rect (tile-dims/2)
		 (ecase (dorito-size d)
		   (:small (both-v (tiles 3/5)))
		   (:medium (both-v (tiles 3/4)))
		   (:large (both-v (tiles 1))))))

(defun dorito-stage-collisions (dorito stage)
  (let ((reverse-x (collision-lambda
		     (setf (x (dorito-vel dorito)) (- (x (dorito-vel dorito)))))))
    (stage-collisions ((dorito-pos dorito) (rect->collision-rects (dorito-collision-rect dorito)) stage)
      :bottom
      (collision-lambda
	(setf (y (dorito-vel dorito)) (- dorito-bounce-speed))
	(push-sound :dorito-bounce))

      :right reverse-x
      :left reverse-x

      :top
      (collision-lambda
	(maxf (y (dorito-vel dorito)) 0)))))

(defstructure particle
    cycler
  sheet-key
  tile-y
  pos
  dead?)

(defun create-particle (&key seq fps sheet-key tile-y pos)
  (let* ((p (make-particle :sheet-key sheet-key
			   :tile-y tile-y
			   :pos pos))
	 (dead?-fn (curry #'particle-dead? p)))
    (setf (particle-cycler p)
	  (create-once-through-cycler 14 seq (lambda () (tf (particle-dead? p)))))
    (def-entity-drawable (() (particle-draw p)))
    p))

(defun particle-draw (particle)
  (draw-sprite :particle (particle-sheet-key particle)
	       (tile-rect (tile-v (funcall (particle-cycler particle))
				  (particle-tile-y particle)))
	       (particle-pos particle)))

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
		(create-timer :length time :ms-remaining time)))
  origin-fn
  amt)

(defun draw-floating-number (fn)
  (draw-number (+v (funcall (floating-number-origin-fn fn))
		   (offset-motion-offset (floating-number-offset fn)))
	       (floating-number-amt fn)))

(defvar damage-numbers)

(defun update-damage-number-amt (e origin-fn amt)
  (aif (gethash e damage-numbers)
       (progn
	 (reset-timer (floating-number-life-timer it))
	 (decf (floating-number-amt it) amt))
       (let* ((dn (make-floating-number :origin-fn origin-fn
					:amt (- amt)))
	      (dead?-fn (curry #'floating-number-dead? dn)))
	 (setf (gethash e damage-numbers) dn)
	 (def-entity-drawable
	     (() (draw-floating-number dn)))

	 (def-entity-physics
	     (() (floating-number-physics dn))))))

(defun floating-number-physics (fn)
  (fnf (floating-number-offset fn) #'offset-motion-physics))

(defun bat-physics (bat)
  (fnf (bat-wave-motion bat) #'wave-physics))

(defun floating-number-dead? (fn)
  (not (timer-active? (floating-number-life-timer fn))))

(defun remove-all-dead ()
  (setf projectile-groups
	(remove-if #'null
		   (loop for (name dead? . g) in projectile-groups
		      collecting
			(let ((new-g (remove-if dead? g)))
			  (when new-g
			    (list* name dead? new-g))))))

  (dohash (k fn) damage-numbers
    (when (floating-number-dead? fn)
      (remhash k damage-numbers))))

(defun draw-bat (bat)
  (let* ((pos (bat-pos bat))
	 (facing (bat-facing bat))
	 (bat-cycle (bat-cycle bat)))
    (draw-sprite :enemy :npc-cemet
		 (tile-rect (tile-v (+ 2 (anim-cycle-current bat-cycle))
				    (if (eq facing :left) 2 3)))
		 pos)))

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
   "I'm on the Mimiga side and anot gonna lose to you!!"
   :wait))

(defun slow-text-speed ()
  (setf text-speed 100))
(defun fast-text-speed ()
  (setf text-speed 25))
(defparameter text-speed 100)
(defparameter cursor-blink-time 100)

(defun create-text-display (pos text)
  (let* ((entity-system-type :dialog)
	 (dead?)
	 (dead?-fn (lambda () dead?))
	 (num-chars 0)
	 (timer (create-expiring-timer text-speed dead?-fn t))
	 (wait-for-input? t)
	 (wait-time 0))

    (def-entity-ai
	(()
	 (cond
	   ((= num-chars (length text))
	    (incf wait-time frame-time)
	    (when (and wait-for-input?
		       (> 2 (chunk-time-period wait-time cursor-blink-time 5)))
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
	    (reset-timer timer)))))

    (def-entity-drawable
	(()
	 (draw-textbox 5 21 30 8)
	 (draw-text-line pos (subseq text 0 num-chars))))))

(defun create-jenka (player)
  (let* ((dead?)
	 facing
	 (pos (tile-v 24 9))
	 (dead?-fn (lambda () dead?))
	 (cycle (create-anim-cycle :fps 3/2
				   :seq #(0 1)
				   :dead?-fn dead?-fn)))
    (def-entity-ai
	(()
	 (setf facing (face-player pos player))))
    (def-entity-drawable
	(()
	 (draw-sprite :npc :npc-regu (tile-rect (tile-v (+ 11 (anim-cycle-current cycle)) (if (eq facing :left) 2 3))) pos)))))

(defun create-hud (player current-gun-exp-fn)
  ;; Player and current-gun-exp-fn are references to outside entities.
  (let* (;; State-vars
	 last-health-amt
	 (dead?-fn (lambda () (player-dead? player)))
	 (timer (create-expiring-timer (s->ms 1) dead?-fn))
	 (health-change-timer (create-expiring-timer (s->ms 1/2) dead?-fn)))

    (def-entity-drawable
	(()
	 (let ((bar-tile/2-w 5)
	       (bar-tile/2-x 5))
	   (draw-hud-sprite
	    :hud-bg :text-box
	    (create-rect-cmpts 0 (tiles/2 5) (tiles/2 8) (tiles/2 1))
	    (tile-v 1 2))

	   (let ((health (player-health-amt player)))
	     (when (timer-active? health-change-timer)
	       (draw-hud-sprite
		:hud :text-box
		(create-rect-cmpts 0 (tiles/2 4)
				   (floor (* (tiles/2 bar-tile/2-w)
					     (/ last-health-amt
						(player-max-health-amt player))))
				   (tiles/2 1))
		(tiles/2-v bar-tile/2-x 4)))
	     (draw-hud-sprite
	      :hud-fg :text-box
	      (create-rect-cmpts 0 (tiles/2 3)
				 (floor (* (tiles/2 bar-tile/2-w)
					   (/ health
					      (player-max-health-amt player))))
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

	     (mvbind (exp gun-name) (funcall current-gun-exp-fn)
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

    ;; My interface.
    (values (lambda () (reset-timer timer))
	    (lambda ()
	      (setf last-health-amt (player-health-amt player))
	      (reset-timer health-change-timer)))))

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

(defvar active-update-systems (list :game))
(defvar active-input-system :game)
(defvar active-draw-systems (list :game))

(defun update-and-render (renderer stage player)
  "The Main Loop, called once per FRAME-TIME."

  (nilf render-list screen-render-list)

  (update-timer-subsystem active-update-systems)
  (update-ai-subsystem active-update-systems)
  (update-physics-subsystem active-update-systems)
  (update-bullet-subsystem active-update-systems)
  (update-stage-collision-subsystem active-update-systems stage)
  (update-pickup-subsystem active-update-systems player)

  (update-damage-collision-subsystem active-update-systems player)
  (update-dynamic-collision-subsystem active-update-systems player)

  (update-drawable-subsystem active-draw-systems)

  (remove-all-dead)

  ;; Debug Drawings Below.

  (draw-point (player-nozzle-pos player) red)
  (draw-point (camera-focus camera) cyan)
  (let ((camera-bounds (stage-dims->camera-bounds (stage-dims stage))))
    (draw-point (clamp-pos (camera-focus camera) camera-bounds) red)
    (draw-rect camera-bounds cyan))
  (draw-point (camera-target-from-player player) white)

  ;; End Debug Drawings.

  (play-sounds sfx-play-list)
  (nilf sfx-play-list))

(defparameter dialog-text-pos (tiles/2-v 7 24))

(defvar reset-vars (make-hash-table))
(defmacro def-reset-var (name init-form)
  `(progn
     (defvar ,name)
     (setf (gethash ',name reset-vars) (lambda () ,init-form))))

(def-reset-var paused? nil)
(def-reset-var player nil)
(def-reset-var active-update-systems (list :game))
(def-reset-var active-draw-systems (list :game))
(def-reset-var active-input-system :game)
(def-reset-var projectile-groups nil)
(def-reset-var input (make-input))
(def-reset-var gun-exps (loop for g across gun-names collecting (cons g 0)))
(def-reset-var damage-numbers (make-hash-table :test 'eq))

(defun reset ()
  (dohash (k v) reset-vars
    (setf (symbol-value k) (funcall v)))

  (switch-to-new-song :weed)
  (set-music-volume 20)

  (comment-code
    (nilf paused?)
    (setf active-update-systems (list :game))
    (setf active-draw-systems (list :game))
    (setf active-input-system :game)
    (nilf player)
    (nilf projectile-groups)
    (setf input (make-input))
    (setf gun-exps (loop for g across gun-names collecting (cons g 0)))
    (setf stage (basic-stage))
    (setf damage-numbers (make-hash-table :test 'eq)))

  (dolist (s subsystems)
    (funcall (cdr s)))
  (setf stage (basic-stage))

  (mvbind (hud-exp-flash-fn hud-player-take-damage-fn)
      (create-hud player (lambda ()
			   (let* ((gun-name (player-current-gun-name player))
				  (exp (cdr (assoc gun-name gun-exps))))
			     (values
			      exp
			      gun-name))))

    (setf player (create-default-player
		  hud-player-take-damage-fn
		  (lambda (gun-name amt)
		    (incf (alexandria:assoc-value gun-exps gun-name) amt)
		    (maxf (alexandria:assoc-value gun-exps gun-name) 0)
		    (when (plusp amt)
		      (funcall hud-exp-flash-fn))
		    (minf (alexandria:assoc-value gun-exps gun-name)
			  (exp-for-gun-level gun-name :max))))))
  (setf camera (create-player-camera (v/2 window-dims) (zero-v) player))
  (create-jenka player)

  (create-critter (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) player)
  (dolist (x '(1 3 6 7))
    (create-bat x 7 player))
  (create-dorito (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) (make-v 0 0) :medium))

(defun init ()
  "Called at application startup."
  (sdl:init '(:audio :video :joystick))
  (sdl.ttf:init)
  (setf font (sdl.ttf:open-font "./content/VeraMoBd.ttf" 19))
  (sdl.mixer:open-audio sdl.mixer:+default-frequency+
			sdl.mixer:+default-format+
			2
			4096)

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
  (cleanup-input input)
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

(defstructure bat
    ;; State-vars
    cycle
  origin
  facing
  dead?
  (wave-motion (make-wave-motion :dir :up
				 :amp (tiles 2)
				 :speed (/ 0.0325 frame-time))))

(defun face-player (pos player)
  (if (< (x pos) (x (player-pos player))) :right :left))

(defun create-bat (tile-x tile-y player)
  (let* ((bat (make-bat :origin (tile-v tile-x tile-y)))
	 (dead?-fn (curry #'bat-dead? bat)))

    (setf (bat-cycle bat)
	  (create-anim-cycle :fps 14 :seq #(0 2 1 2)
			     :dead?-fn dead?-fn))

    (def-entity-ai
	(()
	 (setf (bat-facing bat) (face-player (bat-pos bat) player))))
    (def-entity-damage-collision
	(()
	 (bat-damage-collision-rect bat))
	(() 1))

    (def-entity-damageable
	(()
	 (bat-collision-rect bat))
	((bullet-hit-amt)
	 (declare (ignore bullet-hit-amt))
	 (push-sound :enemy-explode)
	 (create-dorito (bat-pos bat) (zero-v) :large)
	 (tf (bat-dead? bat))))

    (def-entity-drawable
	(()
	 (draw-bat bat)))

    (def-entity-physics
	(()
	 (bat-physics bat)))

    bat))

(defun bat-pos (b)
  (+v (wave-offset (bat-wave-motion b)) (bat-origin b)))

(defun bat-damage-collision-rect (b)
  (create-rect (+v (bat-pos b) (tile-dims/2))
	       (both-v 1)))

(defun bat-collision-rect (b)
  (create-rect (bat-pos b) (tile-dims)))

(defparameter critter-dynamic-collision-rect
  (make-rect :pos (tile-v 0 1/4) :size (tile-v 1 3/4)))

(defun create-critter (pos player)
  ;; Pos: State-var
  ;; Player: referenced-entity
  (let* ((collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6))
	 ;; State-vars
	 dead?
	 (id (gensym))
	 (vel (make-v 0 0))
	 ground-tile
	 facing

	 ;; My-interface
	 (dead?-fn (lambda () dead?))
	 ;; Referenced entities
	 (sleep-timer (create-expiring-timer 300 dead?-fn)))

    (def-entity-ai
	(()
	 (setf facing (face-player pos player))
	 (let ((disp (- (x pos) (x (player-pos player)))))
	   (when (and ground-tile
		      (not (timer-active? sleep-timer))
		      (< (abs disp) (tiles 2)))
	     (setf (x vel) (* 0.04 (- (signum disp))))
	     (setf (y vel) -0.35)))))

    (def-entity-drawable
	(()
	 (let ((sprite-tile-x (if (< (abs (- (x pos) (x (player-pos player)))) (tiles 4))
				  1
				  0)))
	   (draw-sprite :enemy :npc-cemet (tile-rect (tile-v sprite-tile-x (if (eq facing :left) 0 1))) pos))))

    (def-entity-physics
	(()
	 (physics-2d pos vel
		     (const-accelerator 0)
		     (const-accelerator gravity-acc)
		     :clamper-vy
		     (clamper+- terminal-speed))))

    (def-entity-damageable
	(()
	 (tile-rect pos))
	((amt)
	 (update-damage-number-amt id (lambda () (+v (tile-dims/2) pos)) amt)
	 (push-sound :enemy-hurt)))

    (def-entity-damage-collision
	(()
	 (tile-rect pos))
	(() 1))

    (def-entity-dynamic-collision
	(() (rect-offset critter-dynamic-collision-rect pos))
	(() vel)
      ((side player-collision-rect player)
       (let ((player-rect (rect-offset player-collision-rect (player-pos player))))
	 (case side
	   (:bottom
	    (when (and (not (player-on-ground? player))
		       (<= (y pos) (bottom player-rect) (+ (y pos) (tiles 1/2))))
	      (setf (player-ground-tile player) :dynamic)
	      (setf (player-ground-inertia player) (lambda () vel))
	      (setf (y (player-vel player)) 0)

	      (setf (player-pos player)
		    (-v
		     (flush-rect-pos player-rect (y (+v (rect-pos critter-dynamic-collision-rect) pos)) :up)
		     (rect-pos player-collision-rect)))))
	   ((:left :right)
	    (let ((disp (- (x (player-pos player)) (x pos))))
	      (when (> (abs disp) (tiles 1/4))
		(setf (x (player-vel player))
		      (* (/ terminal-speed 70) disp)))))))))

    (def-entity-stage-collision
	((stage)
	 (let (new-tile)
	   (stage-collisions (pos collision-rects stage)
	     :bottom
	     (collision-lambda
	       (setf new-tile tile-type)
	       (unless ground-tile
		 (reset-timer sleep-timer))
	       (allf 0 (y vel) (x vel)))

	     :right (collision-lambda)
	     :left (collision-lambda)

	     :top
	     (collision-lambda
	       (maxf (y vel) 0)))
	   (setf ground-tile new-tile))))))
