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

(defparameter window-dims (make-v 640 480))

;; Global for top-level inspection.
(defvar stage)
(defparameter player (make-player))

;; Debug params.
(defparameter paused? nil)
(defparameter update-period 1
  "Number of frames per update. 1 for real-time.")

(defvar *mus-intro*)
(defvar *mus-loop*)

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
		  (when (>= frame-timer (* update-period frame-time))
		    (unless paused?
		      (update-and-render renderer stage player))
		    (decf frame-timer (* update-period frame-time)))
		  (incf frame-timer (- (sdl:get-ticks) last-update-time))
		  (setf last-update-time (sdl:get-ticks))
		  (when (= 0 (sdl.mixer:playing-music))
		    (sdl.mixer:play-music *mus-loop* -1))
		  (sdl:delay 1))))
      (cleanup))))

(defparameter projectile-groups nil)

(defun map-projectile (projectile-groups fn)
  (dolist (grp projectile-groups)
    (dolist (p (cdr grp))
      (funcall fn p))))

(defun make-projectile-group (gun-name lvl dir nozzle-pos)
  (case gun-name
    (:polar-star
     (make-polar-star-projectile-group lvl dir nozzle-pos))
    (:missile-launcher
     (make-missile-projectile-group lvl dir nozzle-pos))))

(defun player-fire-gun (player)
  (let ((gun-name (player-current-gun-name player)))
    (let ((num-projectile-groups (count gun-name projectile-groups :key #'car))
	  (nozzle-pos (add-v (nozzle-offset player gun-name)
			     (gun-pos player)))
	  (dir (aif (player-actual-v-facing player)
		    it
		    (player-h-facing player)))
	  ;; TODO: Determine the level of the gun.
	  (lvl 2)
	  (max-projectiles (cdr (assoc gun-name max-projectile-groups))))
      (unless (null max-projectiles)
	(when (< num-projectile-groups max-projectiles)
	  (push (cons gun-name (make-projectile-group gun-name lvl dir nozzle-pos))
		projectile-groups))))))

(defparameter player-hop-speed (/ player-jump-speed 1.5))
(defun player-take-damage (p)
  (unless (plusp (player-invincible-time-remaining p))
    (setf (player-invincible-time-remaining p) 3000)

    (nilf (player-ground-tile p))
    (setf (y (player-vel p))
	  (min (- player-hop-speed) (y (player-vel p))))))

(defun handle-input (input renderer stage player)
  "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."
  (gather-input input)
  (when (any? (held-keys input :capslock :escape))
    (quit))
  (when (key-pressed? input :p)
    (togglef paused?))

  (when (key-pressed? input :f1)
    (togglef render-debug?))
  (player-input player input)
  (when (key-pressed? input :x)
    ;; Fire Gun
    (player-fire-gun player))

  (when (key-pressed? input :a)
    ;; Previous gun.
    ;; TODO: Two-way cycleing. Not time-based.
    (let ((curr-idx (position (player-current-gun-name player) gun-names)))
      (decf curr-idx)
      (when (< curr-idx 0)
	(setf curr-idx (1- (length gun-names))))
      (setf (player-current-gun-name player) (aref gun-names curr-idx))))

  (when (key-pressed? input :s)
    ;; Next gun.
    (let ((curr-idx (position (player-current-gun-name player) gun-names)))
      (incf curr-idx)
      (when (= curr-idx (length gun-names))
	(setf curr-idx 0))
      (setf (player-current-gun-name player) (aref gun-names curr-idx))))

  (when (and paused? (key-pressed? input :n))
    (update-and-render renderer stage player)))

(defgeneric draw (o))

(defmethod draw ((p player))
  (player-draw p))

(defstruct bat
  (cycle (create-cycle :fps 14 :data '(0 2 1 2)))
  (rads (rand-angle))
  origin
  dead?)

(defparameter bats
  (list
   (make-bat :origin (tile-pos->pos (make-v 7 7)))
   (make-bat :origin (tile-pos->pos (make-v 6 7)))
   (make-bat :origin (tile-pos->pos (make-v 3 7)))
   (make-bat :origin (tile-pos->pos (make-v 1 7)))))

(defun bat-pos (b)
  (add-v (bat-origin b)
	 (make-v 0 (* 2 tile-size (sin (bat-rads b))))))

(defun bat-damage-collision-rect (b)
  (let ((tile-center (add-v (bat-pos b) (both-v (/ tile-size 2)))))
    (make-rect :pos (make-v (x tile-center) (y tile-center))
	       :size (make-v 1 1))))

(defun bat-collision-rect (b)
  (make-rect :pos (bat-pos b)
	     :size (tile-pos->pos (make-v 1 1))))

(defun rects-collide? (a b)
  (and (>= (right a)  (left b))
       (<= (left a)   (right b))
       (<= (top a)    (bottom b))
       (>= (bottom a) (top b))))

;; TODO: Colors
;; TODO: Layers for debug rendering
;; TODO: Turn on/off render layers.
;; TODO: Create A Timer system of some kind.

(defun update-and-render (renderer stage player)
  "The Main Loop, called once per FRAME-TIME."
  (player-physics player)

  (when (player-walking? player)
    (let* ((wc (player-walk-cycle player))
	   (cur (cycle-current wc)))
      (cycle-update (player-walk-cycle player))
      (when (and (not (eq cur (cycle-current wc)))
		 (= 0 (player-walk-idx player)))
	(push (make-sound :key :step)
	      sfx-play-list))))

  (when (plusp (player-invincible-time-remaining player))
    (decf (player-invincible-time-remaining player) frame-time))

  (draw-stage stage)

  (push-debug-render (make-rect-drawing :color #(255 0 0 255)
					:rect (centered-rect (add-v (nozzle-offset player (player-current-gun-name player))
								    (gun-pos player))
							     (both-v 5))
					:filled? t))

  (dolist (bat bats)
    (unless (bat-dead? bat)
      (let* ((pos (bat-pos bat))
	     (facing (if (< (x pos) (x (player-pos player)))
			 :right
			 :left))
	     (rad-speed 0.0325)
	     (bat-cycle (bat-cycle bat)))
	(cycle-update bat-cycle)
	(incf (bat-rads bat) rad-speed)

	(push-render (make-sprite-drawing :layer :enemy
					  :sheet-key :npc-cemet
					  :src-rect (tile-rect (make-v (+ 2 (car (cycle-current bat-cycle)))
								       (if (eq facing :left)
									   2
									   3)))
					  :pos pos)))))

  (map-projectile projectile-groups #'projectile-physics)

  ;;; Map Collisions
  ;; Player-Map Collisions
  (player-collisions player stage)

  ;; Projectile-Map Collisions
  (map-projectile projectile-groups (rcurry #'projectile-stage-collisions stage))

  ;;; Dynamic Collisions
  ;; Check Projectile-Enemy Collisions
  (map-projectile
   projectile-groups
   (lambda (p)
     (dolist (bat bats)
       (let ((collision-rect (bat-collision-rect bat)))
	 (unless (or (projectile-dead? p)
		     (bat-dead? bat))
	   (when (rects-collide? collision-rect (projectile-collision-rect p))
	     (projectile-kill p)
	     (tf (bat-dead? bat))
	     (push-debug-render (make-rect-drawing :color #(255 255 255 255)
						   :rect collision-rect
						   :filled? t))))))))

  ;; Check Player-Enemy Collisions
  (dolist (bat bats)
    (let ((collision-rect (bat-damage-collision-rect bat)))
      (unless (bat-dead? bat)
	(when (rects-collide? collision-rect (player-damage-collision-rect player))
	  (player-take-damage player)
	  (push-debug-render (make-rect-drawing :color #(255 255 255 255)
						:rect collision-rect
						:filled? t))))))

  ;;; Remove the dead
  (setf projectile-groups
	(remove-if #'null
		   (loop for grp in projectile-groups
		      collecting
			(cons (car grp) (remove-if #'projectile-dead? (cdr grp))))
		   :key #'cdr))

  (setf bats (remove-if #'bat-dead? bats))

  (draw player)

  (map-projectile projectile-groups #'draw)

  (play-sounds sfx-play-list)
  (nilf sfx-play-list)
  (render renderer render-list)
  (nilf render-list debug-render-list))

(defun init ()
  "Called at application startup."
  (sdl:init :everything)
  (sdl.ttf:init)
  (sdl.mixer:open-audio sdl.mixer:+default-frequency+
			sdl.mixer:+default-format+
			2
			4096)

  (sdl:show-cursor :disable)

  (put-spritesheets spritesheet-fnames)
  (put-sfx sfx-fnames)

  (mvsetq
   (window renderer)
   (sdl:default-window-and-renderer
       "Cave Story"
       (x window-dims) (y window-dims)
       0))

  (progn
    (setf *mus-intro* (sdl.mixer:load-mus "/home/chebert/Projects/lisp/cave-story/content/remastered-music/curly_intro.ogg"))
    (setf *mus-loop* (sdl.mixer:load-mus "/home/chebert/Projects/lisp/cave-story/content/remastered-music/curly_loop.ogg"))
    (sdl.mixer:play-music *mus-intro* 0))

  (setf input (make-input))
  (setf player (make-player))
  (setf stage (basic-stage)))

(defun cleanup ()
  "Called at application closing to cleanup all subsystems."
  (cleanup-input input)
  (cleanup-spritesheets)
  (sdl.mixer:free-music *mus-intro*)
  (sdl.mixer:free-music *mus-loop*)
  (sdl.mixer:close-audio)
  (sdl:destroy-renderer renderer)
  (sdl:destroy-window window)
  (sdl.ttf:quit)
  (sdl:quit)
  (nilf renderer window))

(defun quit ()
  "Quits the application."
  (throw 'exit nil))
