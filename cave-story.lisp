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
(defvar hud)
(defvar projectile-groups)

;; Debug params.
(defparameter paused? nil)
(defparameter update-period 1
  "Number of frames per update. 1 for real-time.")

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
		  (when (key-pressed? input :r)
		    (reset))
		  (when (>= frame-timer (* update-period frame-time))
		    (unless paused?
		      (update-and-render renderer stage player))
		    (render renderer render-list (camera-focus->camera-pos
						  (clamp-pos (camera-focus camera) (stage-dims->camera-bounds (stage-dims stage)))))
		    (decf frame-timer (* update-period frame-time)))
		  (incf frame-timer (- (sdl:get-ticks) last-update-time))
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

    (player-input player input)
    (when (or (member 1 (ti-pressed-joy-buttons (input-transient-input input)))
	      (key-pressed? input :x))
      ;; Fire Gun
      (player-fire-gun player))

    (when (or (key-pressed? input :a) (member 3 (ti-pressed-joy-buttons (input-transient-input input))))
      (cycle-previous (player-gun-name-cycle player)))

    (when (or (key-pressed? input :s) (member 2 (ti-pressed-joy-buttons (input-transient-input input))))
      (cycle-next (player-gun-name-cycle player)))

    (when (and paused? (key-pressed? input :n))
      (update-and-render renderer stage player))))

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

(defstruct dorito
  cycle
  pos
  vel
  size
  (life-timer (let ((time (s->ms 8)))
		(create-timer :length time :ms-remaining time)))
  dead?)

(defun create-dorito (pos vel size)
  (let* ((d (make-dorito :pos pos
			 :size size
			 :vel vel))
	 (dead?-fn (curry #'dorito-dead? d)))
    (setf (dorito-cycle d) (create-anim-cycle :fps 14 :seq (alexandria:iota 6)
					      :dead?-fn dead?-fn))

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
	 (tf (dorito-dead? d))))
    d))

(defun dorito-draw (d)
  (let ((life-tr (dorito-life-timer d)))
    (unless (timer-active? (dorito-life-timer d))
      (tf (dorito-dead? d)))
    (unless (and (< (timer-ms-remaining life-tr) (s->ms 1))
		 (zerop (chunk-time-period life-tr 50)))
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

(defstruct particle
  cycle
  sheet-key
  tile-y
  pos
  dead?)

(defun create-particle (&key seq fps sheet-key tile-y pos)
  (let ((p (make-particle :sheet-key sheet-key
			  :tile-y tile-y
			  :pos pos)))
    (setf (particle-cycle p)
	  (create-once-through-cycle 14 seq (lambda () (tf (particle-dead? p)))))
    (register-drawable :draw-fn (curry #'particle-draw p) :dead?-fn (curry #'particle-dead? p))
    p))

(defun particle-draw (particle)
  (draw-sprite :particle (particle-sheet-key particle)
	       (tile-rect (tile-v (cycle-current (particle-cycle particle))
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

(defstruct floating-number
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
	 (incf (floating-number-amt it) amt))
       (let ((dn (make-floating-number :origin-fn origin-fn
				       :amt amt)))
	 (setf (gethash e damage-numbers) dn)
	 (register-drawable :draw-fn (curry #'draw-floating-number dn)
			    :dead?-fn (curry #'floating-number-dead? dn))
	 (register-physics :physics-fn (curry #'floating-number-physics dn)
			   :dead?-fn (curry #'floating-number-dead? dn)))))

(defun floating-number-physics (fn)
  (offset-motion-physics (floating-number-offset fn)))

(defun bat-physics (bat)
  (wave-physics (bat-wave-motion bat)))


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

(defun create-hud (player)
  (let ((hud nil)
	(dead?-fn (lambda () (player-dead? player))))
    (def-entity-drawable
	(()
	 (draw-hud-sprite
	  :hud-bg :text-box
	  (create-rect-cmpts 0 (tiles/2 5) (tiles/2 8) (tiles/2 1))
	  (tile-v 1 2))

	 (let ((health (player-health-amt player)))
	   (draw-number (tiles/2-v (if (< health 10) 4 3) 4)
			health
			:centered? nil
			:show-sign? nil
			:push-fn #'push-screen-render))))
    hud))

(defun update-and-render (renderer stage player)
  "The Main Loop, called once per FRAME-TIME."

  (nilf render-list screen-render-list)

  (update-timer-subsystem)
  (update-ai-subsystem)
  (update-physics-subsystem)
  (update-bullet-subsystem)
  (update-stage-collision-subsystem stage)
  (update-pickup-subsystem player)

  (update-damage-collision-subsystem player hud)
  (update-dynamic-collision-subsystem player)
  (update-drawable-subsystem)

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

(defun reset ()
  (nilf paused?)
  (switch-to-new-song :curly)
  (set-music-volume 20)

  (dolist (s subsystems)
    (funcall (cdr s)))
  (nilf projectile-groups)
  (setf input (make-input))
  (setf player (create-player))
  (setf stage (basic-stage))

  (setf camera (create-player-camera (v/2 window-dims) (zero-v) player))
  (setf damage-numbers (make-hash-table :test 'eq))

  (setf hud (create-hud player))
  (create-critter (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) player)
  (dolist (x '(1 3 6 7))
    (create-bat x 7 player))
  (create-dorito (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) (make-v 0 0) :medium))

(defun init ()
  "Called at application startup."
  (sdl:init '(:audio :video :joystick))
  (sdl.ttf:init)
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

  (sdl.mixer:close-audio)
  (sdl:destroy-renderer renderer)
  (sdl:destroy-window window)
  (sdl.ttf:quit)
  (sdl:quit)
  (nilf renderer window))

(defun quit ()
  "Quits the application."
  (throw 'exit nil))

(defstruct bat
  cycle
  origin
  facing
  (wave-motion (make-wave-motion :dir :up
				 :amp (tiles 2)
				 :speed (/ 0.0325 frame-time)))
  dead?)

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
	(() -1))
    (def-entity-damageable
	(()
	 (bat-collision-rect bat))
	((bullet-hit-amt)
	 (declare (ignore bullet-hit-amt))
	 (push-sound :enemy-explode)
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
  (let* (dead?
	 (id (gensym))
	 (vel (make-v 0 0))
	 (dead?-fn (lambda () dead?))
	 (collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6))
	 (sleep-timer (create-expiring-timer 300 dead?-fn))
	 ground-tile
	 facing)

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
	(() -1))

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
