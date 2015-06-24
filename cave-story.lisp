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

(defvar global-game)

(defparameter *debug-input-keybindings*
  `((((:key :capslock :escape)) .
     ,(lambda () (quit)))
    (((:key :p)
      (:joy :start)) .
     ,(lambda () (togglef global-paused?)))
    (((:key :r)
      (:joy :select)) .
     ,(lambda () (setq global-game (reset))))
    (((:joy :r)) .
     ,(lambda ()
	      (case *input-playback*
		(:recording (begin-input-playback))
		(:playback (nilf *input-playback*))
		(t (begin-input-recording)))))
    (((:key :n)) .
     ,(lambda ()
	      (when global-paused?
		(fnf global-game (curry #'update-and-render renderer)))))))

(let (debug-toggle-off?)
  (defun handle-debug-input (transient-input)
    (let ((pressed-keys (ti-pressed-keys transient-input))
	  (pressed-joy (ti-pressed-joy-buttons transient-input)))
      (loop for (bindings . action) in *debug-input-keybindings*
	 do (loop for (type . b) in bindings
	       do (when (ecase type
			  (:key (any? (intersection pressed-keys b :test #'eq)))
			  (:joy (any? (intersection pressed-joy (mapcar #'joy-key->num b) :test #'eq))))
		    (funcall action)
		    (return))))

      (when (find :f1 pressed-keys)
	(mapc (if debug-toggle-off?
		  (progn
		    (nilf debug-toggle-off?)
		    #'remove-visible-layer)
		  #'toggle-visible-layer)
	      debug-layers))

      (loop for key in '(:f2 :f3 :f4 :f5 :f6)
	 for i from 0
	 do
	   (when (find key pressed-keys)
	     (let ((layer (elt debug-layers (1+ i))))
	       (print layer)
	       (tf debug-toggle-off?)
	       (toggle-visible-layer layer)))))))

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
		  (let ((transient-input (gather-transient-input)))
		    (handle-debug-input transient-input)
		    (modify-game (global-game)
		      (fnf input (rcurry #'gather-input transient-input))))

		  (when (>= frame-timer (* update-period frame-time))
		    (if global-paused?
			(draw-text-line (zero-v) "PAUSED")
			(fnf global-game (curry #'update-and-render renderer)))
		    (render renderer
			    font
			    render-list
			    (camera-pos
			     (estate (game-camera global-game))
			     (stage-dims->camera-bounds (stage-dims (game-stage global-game)))))
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

(defun handle-input (game &aux input)
  "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."

  (setf input (game-input game))
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
	  (dialog-buttons-released)))))))

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
    (alist
     :top (create-rect (+v (make-v bs 0) pos)
		       (-v size (make-v dbs bs)))

     :bottom (create-rect (+v (make-v bs bs) pos)
			  (-v size (make-v dbs bs)))

     :left (create-rect (+v (make-v 0 bs) pos)
			(-v size (make-v bs dbs)))

     :right (create-rect (+v (make-v bs bs) pos)
			 (-v size (make-v bs dbs))))))

(defun rect->collision-rects (rect &optional (buffer-size 6))
  (collision-rects (rect-pos rect) (rect-size rect) buffer-size))

(defparameter dorito-friction-acc 0.00002)
(defparameter dorito-bounce-speed 0.225)

(defstructure pickup type amt)

(def-entity dorito
    (dead?
     size)
  (create-dorito (pos vel size)
		 (make-dorito
		  :timers
		  (alist
		   :life (create-expiring-timer (s->ms 8) t)
		   :anim-cycle (create-timed-cycle 14 (alexandria:iota 6)))
		  :physics
		  (alist
		   :stage
		   (make-kin-2d :pos (-v pos (rect-pos (dorito-collision-rect size)))
				:vel vel
				:accelerator-x (friction-accelerator dorito-friction-acc)
				:accelerator-y (const-accelerator gravity-acc)
				:clamper-vy (clamper+- terminal-speed)))
		  :size size))
  :timers :physics :stage-collision :drawable :pickup)

(defmacro ai-life-timer ()
  `(unless (timer-active? (aval timers :life))
     (tf dead?)))

(dorito-methodf ai (d ticks)
  (ai-life-timer))

(defun dorito-pos (d)
  (physics-pos d))

(defun flash-time? (tr)
  (and (timer-active? tr) (zerop (chunk-timer-period tr 50))))


(defun death-flash? (timers)
  (let ((tr (aval timers :life)))
    (and (< (timer-ms-remaining tr) (s->ms 1))
	 (flash-time? tr))))

(dorito-method draw (d)
  (unless (death-flash? timers)
    (draw-sprite
     :pickup :npc-sym

     (create-rect
      (+v
       (anim-cycle-offset timers)
       (tile-v 0 (1+ (position size '(:small :medium :large)))))
      (make-v (tiles 1) (1- (tiles 1))))

     (physics-pos d))))

(defun set-x-v (v x)
  (make-v x (y v)))
(defmacro set-x-vf (v x)
  `(setf ,v (set-x-v ,v ,x)))
(defun set-y-v (v y)
  (make-v (x v) y))
(defmacro set-y-vf (v y)
  `(setf ,v (set-y-v ,v ,y)))

(defun reverse-x-v (v)
  (make-v (- (x v)) (y v)))
(defmacro reverse-x-vf (v)
  `(setf ,v (reverse-x-v ,v)))

(dorito-methodf stage-collision (d stage)
  (let ((collision-rects (rect->collision-rects (dorito-collision-rect size))))
    (astage-collisionsf
     (alist
      :bottom
      (collision-lambda
	(set-y-vf vel (- dorito-bounce-speed))
	(push-sound :dorito-bounce))

      :right (collision-lambda
	       (when (plusp (x vel))
		 (reverse-x-vf vel)))
      :left (collision-lambda
	      (when (minusp (x vel))
		(reverse-x-vf vel)))

      :top
      (collision-lambda
	(max-y-vf vel 0))))))

(dorito-method pickup-rect (d)
  (rect-offset (dorito-collision-rect size) (physics-pos d)))

(dorito-methodf pickup-kill (d)
  (push-sound :pickup)
  (tf dead?))

(dorito-method pickup-data (d)
  (make-pickup :type :dorito :amt (ecase size
				    (:small 1)
				    (:medium 10)
				    (:large 20))))

(dorito-method dead? (d)
  dead?)

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
		   (:small (both-v (tiles 2/5)))
		   (:medium (both-v (tiles 3/4)))
		   (:large (both-v (tiles 1))))))

(def-entity single-loop-sprite
    (dead?
     sheet-key
     tile-y
     layer)
  (create-single-loop-sprite (fps seq sheet-key tile-y layer)
			     (make-single-loop-sprite :timers (alist :cycle (create-timed-cycle fps seq))
						      :sheet-key sheet-key
						      :layer layer
						      :tile-y tile-y))
  :timers)

(defun single-loop-sprite-draw (s pos)
  (with-single-loop-sprite-slots (s)
    (draw-sprite layer
		 sheet-key
		 (tile-rect (tile-v (cycle-current (timed-cycle-cycle (aval timers :cycle))) tile-y))
		 pos)))

(single-loop-sprite-methodf ai (p ticks)
  (when (and (find :cycle ticks)
	     (zerop (cycle-idx (timed-cycle-cycle (aval timers :cycle)))))
    (tf dead?)))

(single-loop-sprite-method dead? (p) dead?)

(def-entity particle
    (single-loop-sprite
     pos)
  (create-particle (&key seq fps sheet-key tile-y pos)
		   (make-particle
		    :single-loop-sprite (create-single-loop-sprite
					 fps seq sheet-key tile-y :particle)
		    :pos pos))
  :drawable)

(particle-method draw (p)
  (with-single-loop-sprite-slots ((estate single-loop-sprite))
    (let ((cycle-current (cycle-current (timed-cycle-cycle (aval timers :cycle)))))
      (draw-sprite :particle sheet-key
		   (tile-rect (tile-v cycle-current tile-y))
		   pos))))

(particle-method dead? (p)
  (dead? (estate single-loop-sprite)))

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

(def-entity floating-number
    (entity
     amt)
  (create-floating-number (entity amt)
			  (make-floating-number :entity entity :amt amt
						:timers (alist :life
							       (create-expiring-timer (s->ms 2) t))
						:physics (alist :offset
								(make-offset-motion
								 (zero-v)
								 :up
								 (/ (tiles 1/30) frame-time)))))
  :timers :drawable :physics)

(floating-number-method draw (fn)
  (draw-number (+v (origin (estate entity))
		   (physics-pos fn))
	       amt))

(floating-number-methodf ai (dn ticks)
  (when (< (y (motion-pos (cdr (assoc :offset physics)))) (- (tiles 1)))
    (asetf physics (make-offset-motion (zero-v :y (- (tiles 1))) :up 0) :offset)))

(floating-number-method dead? (dn)
  (not (timer-active? (aval timers :life))))

(defun floating-number-add-amt (dn amount)
  (modify-floating-number (dn)
    (aupdatef timers #'reset-timer '(:life))
    (incf amt amount)))

(defun remove-all-dead (game)
  (replace-entity-state (game-projectile-groups game) #'projectile-groups-remove-dead)
  (replace-entity-state (game-damage-numbers game) #'damage-numbers-remove-dead))

(defun draw-bat (pos facing cycle-idx)
  (draw-sprite :enemy
	       :npc-cemet
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

(def-entity text-display
    (pos
     text
     num-chars
     wait-for-input?
     blink-time
     dead?)
  (create-text-display (pos text)
		       (make-text-display :pos pos
					  :text text
					  :num-chars 0
					  :timers (alist :text (create-expiring-timer text-speed t))
					  :wait-for-input? t
					  :blink-time 0))
  :timers :drawable)

(text-display-methodf ai (td ticks)
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
    ((not (timer-active? (aval timers :text)))
     (incf num-chars)
     (push-sound :text-click)
     (asetf timers (create-expiring-timer text-speed t) :text))))

(text-display-method draw (td)
  (draw-textbox 5 21 30 8)
  (draw-text-line pos (subseq text 0 num-chars)))

(text-display-method dead? (td) dead?)

(def-entity hud
    (player
     gun-exps

     last-health-amt)
  (create-hud (player gun-exps id)
	      (values (make-hud :player player :gun-exps gun-exps
				:timers (alist
					 :exp-change (create-expiring-timer (s->ms 1))
					 :health-change (create-expiring-timer (s->ms 1/2))))
		      id))
  :timers :drawable)

(hud-method draw (h)
  (let ((bar-tile/2-w 5)
	(bar-tile/2-x 5))
    (draw-hud-sprite
     :hud-bg :text-box
     (create-rect-cmpts 0 (tiles/2 5) (tiles/2 8) (tiles/2 1))
     (tile-v 1 2))

    (let ((health (player-health-amt (player-state player))))
      (when (timer-active? (aval timers :health-change))
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

      (when (flash-time? (aval timers :exp-change))
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

	  (draw-hud-number 3 (1+ current-level)))))))

(hud-method dead? (h)
  (dead? (estate player)))

(defun hud-exp-changed (h)
  (modify-hud (h)
    (aupdatef timers #'reset-timer '(:exp-change))))

(defun hud-health-changed (h)
  (modify-hud (h)
    (setf last-health-amt (player-health-amt (player-state player)))
    (aupdatef timers #'reset-timer '(:health-change))))

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
  (when (eq *input-playback* :playback)
    (modify-game (global-game)
      (setf input (next-playback-input))))
  (handle-input global-game)

  (nilf render-list screen-render-list)
  (update-parameter-subsystem entity-systems)

  (case *input-playback*
    (:recording
     (draw-text-line (zero-v) "RECORD"))
    (:playback
     (draw-text-line (zero-v) "PLAYBACK")))

  (let ((active-update-systems (active-systems-update (estate (game-active-systems game))))
	(stage (game-stage game))
	(player (game-player game)))
    (update-timers-subsystem active-update-systems)
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
  (nilf sfx-play-list)
  (modify-game (game)
    (when (eq *input-playback* :recording)
      (record-frame-input input))
    (modify-input (input)
      (fnf transient-input #'clear-transient-input))))

(defparameter *input-playback* nil)

(let (inputs playback-idx game-state)
  (defun record-frame-input (input)
    (push input inputs))

  (defun next-playback-input ()
    (prog1
	(elt inputs (decf playback-idx))
      (when (= 0 playback-idx)
	(restore-state game-state)
	(setf playback-idx (1- (length inputs))))))

  (defun end-input-playback ()
    (nilf *input-playback*))

  (defun begin-input-playback ()
    (restore-state game-state)
    (setf playback-idx (1- (length inputs)))
    (setf *input-playback* :playback))

  (defun begin-input-recording ()
    (setf *input-playback* :recording)
    (nilf inputs)
    (setf game-state (save-current-state))))

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
  (list (current-entity-states) (copy-game global-game)))

(defun restore-state (state)
  (dolist (s registry-syms)
    (nilf (symbol-value s)))
  (restore-entity-states (first state))
  (setf global-game (second state)))

(defun create-game ()
  (let ((damage-numbers (create-damage-numbers))
	(projectile-groups (create-projectile-groups))
	(stage (basic-stage))
	(hud (gen-entity-id))
	(gun-exps (create-gun-exps))
	(active-systems (create-active-systems)))
    (let* ((player (create-default-player hud projectile-groups damage-numbers gun-exps active-systems))
	   (camera (create-player-camera (v/2 window-dims) (zero-v) player)))
      (create-hud player gun-exps hud)

      ;;-critter (make-v (+ (tiles 14) (tiles 1/4)) (tiles 6)) player damage-numbers)
      (create-elephant (make-v (tiles 7) (tiles 6)) player camera damage-numbers)
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

  (nilf *input-playback*)
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
  (if (< (x pos) (x (physics-pos (player-state player)))) :right :left))

(def-entity bat
    (player
     facing
     health-amt
     damage-numbers
     id
     dead?)
  (create-bat (tile-x tile-y player)
	      (make-bat :physics
			(alist :wave
			       (make-wave-motion
				:origin (tile-v tile-x tile-y)
				:dir :up
				:amp (tiles 2)
				:speed (/ 0.0325 frame-time)))
			:timers
			(alist :anim-cycle (create-timed-cycle 14 #(0 2 1 2)))
			:health-amt 1
			:player player))
  :timers :damage-collision :damageable :drawable :physics)
(enemy-damageable-hit-react-method bat 3)
(bat-method dead? (b) dead?)

(defmethod origin ((b bat))
  (physics-tile-origin b))

(bat-method draw (b)
  (draw-sprite :enemy
	       :npc-cemet
	       (tile-rect (+v (tile-v 2 2)
			      (anim-cycle-offset timers)
			      (facing-offset facing)))
	       (physics-pos b)))

(defmacro ai-face-player (e)
  `(setf facing (face-player (physics-pos ,e) player)))

(bat-methodf ai (b ticks)
  (ai-face-player b))

(defun facing-offset (facing)
  (tile-v 0 (if (eq facing :left) 0 1)))

(defun anim-cycle-offset (timers)
  (tile-v (timed-cycle-current (aval timers :anim-cycle)) 0))

(defun anim-cycle-val (timers)
  (timed-cycle-current (aval timers :anim-cycle)))

(defun point-rect (pos)
  (create-rect pos (both-v 1)))

(defmethod damage-collision-rect ((b bat))
  (point-rect (origin b)))

(defmethod damage-collision-amt ((b bat)) 1)
(defmethod damageable-rect ((b bat))
  (physics-tile-rect b))

(defun polar-vec->v (angle mag)
  (make-v (* mag (cos angle))
	  (* mag (sin angle))))

(def-entity death-cloud-particle
    (single-loop-sprite)
  (create-death-cloud-particle (pos)
			       (make-death-cloud-particle
				:single-loop-sprite
				(create-single-loop-sprite
				 15 (mapcar #'1+ (alexandria:iota 7))
				 :npc-sym 0 :particle)
				:physics
				(alist
				 :stage
				 (make-kin-2d :pos (-v pos (tile-dims/2))
					      :vel (polar-vec->v (rand-angle) (rand-val-between 0.1 0.3))
					      :clamper-vx (clamper+- terminal-speed)
					      :clamper-vy (clamper+- terminal-speed)))))
  :drawable :physics :stage-collision)

(death-cloud-particle-method draw (d)
  (single-loop-sprite-draw (estate single-loop-sprite) (physics-pos d)))

(death-cloud-particle-method dead? (d)
  (dead? (estate single-loop-sprite)))

(let ((collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 2/5))))))
  (death-cloud-particle-methodf stage-collision (d stage)
    (astage-collisionsf
     (let ((stop-x (collision-lambda (set-x-vf vel 0)))
	   (stop-y (collision-lambda (set-y-vf vel 0))))
       (alist
	:bottom stop-y
	:left stop-x
	:right stop-x
	:top stop-y)))))

(defun create-death-cloud-particles (num pos)
  (dotimes (i num)
    (create-death-cloud-particle pos)))

(defparameter critter-dynamic-collision-rect
  (make-rect :pos (tile-v 0 1/4) :size (tile-v 1 3/4)))

(defgeneric physics-pos (p))

(defun gravity-kin-2d (&key (pos (zero-v)) (vel (zero-v)))
  (make-kin-2d :pos pos
	       :vel vel
	       :accelerator-y (const-accelerator gravity-acc)
	       :clamper-vy (clamper+- terminal-speed)))

(def-entity critter
    (dead?
     health-amt
     ground-tile
     facing

     id
     player
     damage-numbers)

  (create-critter (pos player damage-numbers)
		  (let ((id (gen-entity-id)))
		    (values (make-critter :physics
					  (alist :stage (gravity-kin-2d :pos pos))
					  :player player
					  :health-amt 2
					  :damage-numbers damage-numbers
					  :id id)
			    id)))

  :timers :drawable :physics :damageable :damage-collision :dynamic-collision :stage-collision)

(defmacro ai-jump (x-speed y-speed)
  `(when ground-tile
     (aupdatef
      physics
      (lambda (kin-2d)
	(modify-kin-2d (kin-2d)
	  (setf vel (make-v (* ,x-speed (if (eq facing :left) -1 1)) (- ,y-speed))))) '(:stage))))

(defun origin-dist (a b)
  (dist (origin a) (origin b)))

(defmacro sleeping? ()
  `(aand (aval timers :sleep) (timer-active? it)))

(defmacro ai-shake ()
  `(unless (aand (aval timers :shake) (timer-active? it))
     (removef physics :shake :key #'car :test #'eq)))

(critter-methodf ai (c ticks)
  (ai-shake)
  (ai-face-player c)
  (when (and (not (sleeping?))
	     (< (origin-dist c (estate player)) (tiles 4)))
    (ai-jump 0.04 0.35)))

(critter-method draw (c)
  (let ((sprite-tile-x (cond
			 ((sleeping?)
			  0)
			 ((< (origin-dist c (estate player)) (tiles 7))
			  1)
			 (t
			  0))))
    (draw-sprite :enemy :npc-cemet
		 (tile-rect (+v (tile-v sprite-tile-x 0)
				(facing-offset facing)))
		 (physics-pos c))))

(defmethod damageable-rect ((c critter))
  (physics-tile-rect c))

(defmacro enemy-hurt-react (e num-death-clouds)
  `(if (< amt health-amt)
       (progn
	 (update-damage-number-amt damage-numbers id amt)
	 (push-sound :enemy-hurt)
	 (decf health-amt amt))
       (let ((origin (origin ,e)))
	 (push-sound :enemy-explode)
	 (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	 (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	 (create-death-cloud-particles ,num-death-clouds origin)
	 (tf dead?))))

(defmacro enemy-damageable-hit-react (e num-death-clouds)
  `(progn
     (asetf physics
	    (make-wave-motion :dir :left
			      :amp 2
			      :speed 0.1
			      :rads 0)
	    :shake)
     (asetf timers (create-expiring-timer (s->ms 1/3) t) :shake)
     (enemy-hurt-react ,e ,num-death-clouds)))

(defmacro enemy-damageable-hit-react-method (name num-death-clouds)
  (with-gensyms (obj)
    `(,(symbolicate name '-methodf) damageable-hit-react (,obj amt)
       (enemy-damageable-hit-react ,obj ,num-death-clouds))))

(enemy-damageable-hit-react-method critter 6)

(defmethod damage-collision-rect ((c critter))
  (physics-tile-rect c))

(defmethod damage-collision-amt ((c critter))
  1)

(defmethod dynamic-collision-rect ((c critter))
  (rect-offset critter-dynamic-collision-rect (physics-pos c)))

(defun dynamic-collision-enemy-react (pos origin id dynamic-collision-rect side player-collision-rect player-state)
  (modify-player (player-state p-)
    (aupdatef
     p-physics
     (lambda (p-kin-2d)
       (modify-kin-2d (p-kin-2d p-)
	 (let ((player-rect (rect-offset player-collision-rect p-pos)))
	   (case side
	     (:bottom
	      (when (and (not (player-on-ground? p-ground-tile))
			 (<= (y pos) (bottom player-rect) (+ (y origin))))
		(setf p-ground-tile :dynamic)
		(setf p-ground-inertia-entity id)
		(setf p-vel (zero-v :x (x p-vel)))

		(setf p-pos
		      (-v
		       (flush-rect-pos player-rect (y (rect-pos dynamic-collision-rect)) :up)
		       (rect-pos player-collision-rect)))))
	     ((:left :right)
	      (let ((disp (- (x p-pos) (x pos))))
		(when (> (abs disp) (tiles 1/4))
		  (setf (x p-vel)
			(* (/ terminal-speed 70) disp))))))))) '(:stage))))

(critter-method dynamic-collision-react (c side player-collision-rect player)
  (dynamic-collision-enemy-react
   (physics-pos c)
   (origin c)
   id
   (dynamic-collision-rect c)
   side
   player-collision-rect
   (estate player)))

(defun max-y-v (v max-y)
  (make-v (x v) (max (y v) max-y)))
(defmacro max-y-vf (v max-y)
  `(setf ,v (max-y-v ,v ,max-y)))

(defmacro zero-vf (place &key (x 0) (y 0))
  `(setf ,place (zero-v :x ,x :y ,y)))

(let ((collision-rects (rect->collision-rects (centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6)))
  (critter-methodf stage-collision (c stage)
    (let ((last-tile ground-tile))
      (nilf ground-tile)
      (astage-collisionsf
       (alist
	:bottom
	(collision-lambda
	  (setf ground-tile tile-type)
	  (unless last-tile
	    (asetf timers (create-expiring-timer (s->ms 1/3) t) :sleep))
	  (zero-vf vel))

	:top
	(collision-lambda
	  (max-y-vf vel 0)))))))

(critter-method dead? (c) dead?)

(defun physics-tile-origin (c)
  (+v (physics-pos c) (tile-dims/2)))
(defun physics-tile-rect (c)
  (tile-rect (physics-pos c)))

(defgeneric origin (obj))
(defmethod origin ((c critter))
  (physics-tile-origin c))

(defgeneric inertia-vel (obj))

(defun stage-vel (physics)
  (kin-2d-vel (cdr (assoc :stage physics))))

(critter-method inertia-vel (c)
  (stage-vel physics))

(defparameter elephant-speed 0.08)
(def-entity elephant
    (dead?
     health-amt
     facing

     id
     player
     camera
     damage-numbers)

  (create-elephant (pos player camera damage-numbers)
		   (let ((id (gen-entity-id)))
		     (values (make-elephant :physics
					    (alist :stage
						   (gravity-kin-2d :pos pos
								   :vel (make-v (- elephant-speed) 0)))
					    :timers
					    (alist :anim-cycle
						   (create-timed-cycle 12 #(0 2 4)))
					    :health-amt 8
					    :facing :left
					    :damage-numbers damage-numbers
					    :camera camera
					    :player player
					    :id id)
			     id)))

  :timers :drawable :physics :stage-collision
  :damageable
  :damage-collision
  :dynamic-collision)

(defparameter elephant-dims (make-v (tiles 2) (tiles 3/2)))
(elephant-method draw (e)
  (let ((src-pos
	 (cond
	   ((timer-active? (aval timers :recover))
	    (make-v (tiles (* 2 3)) 0))
	   (t (anim-cycle-offset timers)))))
    (draw-sprite :enemy
		 :npc-eggs1
		 (create-rect (+v src-pos
				  (make-v 0 (if (eq facing :left) 0 (tiles 3/2))))
			      elephant-dims)
		 (physics-pos e))))

(elephant-method origin (e)
  (+v (physics-pos e)
      (scale-v elephant-dims 1/2)))

(elephant-method inertia-vel (e)
  (scale-v (stage-vel physics) 1/3))

(elephant-method damageable-rect (e)
  (create-rect (physics-pos e) elephant-dims))

(elephant-methodf damageable-hit-react (e amt)
  (enemy-damageable-hit-react e amt)
  (unless (timer-active? (aval timers :rage))
    (if (timer-active? (aval timers :recover))
	(when (< (timer-ms-remaining (aval timers :recover)) (s->ms 1/2))
	  (asetf timers (create-expiring-timer (s->ms 3)) :rage))
	(asetf timers (create-expiring-timer (s->ms 3/4) t) :recover))))

(elephant-method dead? (e) dead?)

(elephant-method dynamic-collision-rect (e)
  (let ((pos (physics-pos e)))
    (cond
      ((timer-active? (aval timers :recover))
       (+vf pos (make-v 0 (tiles 1/4))))
      ((= 1 (cycle-idx (timed-cycle-cycle (aval timers :anim-cycle))))
       (+vf pos (make-v 0 (tiles 1/8)))))
    (create-rect pos elephant-dims)))

(elephant-method damage-collision-amt (e)
  (if (timer-active? (aval timers :rage))
      5
      1))
(elephant-method damage-collision-rect (e)
  (let* ((dims (make-v (tiles 1) (tiles 3/4)))
	 (y (tiles 3/4))
	 (pos (if (eq facing :left)
		  (make-v 0 y)
		  (make-v (tiles 1) y))))
    (create-rect (+v (physics-pos e) pos) dims)))

(elephant-method dynamic-collision-react (c side player-collision-rect player)
  (dynamic-collision-enemy-react
   (physics-pos c)
   (origin c)
   id
   (dynamic-collision-rect c)
   side
   player-collision-rect
   (estate player)))

(elephant-methodf ai (e ticks)
  (ai-shake)
  (when (member :recover ticks)
    (when (aval timers :rage)
      (aupdatef timers #'reset-timer '(:rage))
      (asetf timers (create-timed-cycle 14 #(8 10)) :anim-cycle)))
  (when (member :rage ticks)
    (setf timers (arem timers '(:rage)))
    (asetf timers (create-timed-cycle 12 #(0 2 4)) :anim-cycle))
  (when (timer-active? (aval timers :rage))
    (when (and (member :anim-cycle ticks)
	       (zerop (cycle-idx (timed-cycle-cycle (aval timers :anim-cycle)))))
      (push-sound :big-footstep)
      (replace-entity-state camera (rcurry #'timed-camera-shake (s->ms 1/2)))
      (create-death-cloud-particles 3 (+v (physics-pos e) (make-v (if (eq facing :left) (tiles 3/2) (tiles 1/2)) (tiles 5/4))))))

  (aupdatef
   physics
   (lambda (kin-2d)
     (modify-kin-2d (kin-2d k-)
       (let ((vel (copy-v2 k-vel)))
	 (cond
	   ((timer-active? (aval timers :rage))
	    (let ((elephant-speed (* 2 elephant-speed)))
	      (if (eq facing :right)
		  (setf (x vel) elephant-speed)
		  (setf (x vel) (- elephant-speed)))))
	   ((timer-active? (aval timers :recover))
	    (setf (x vel) 0))
	   (t
	    (if (eq facing :right)
		(setf (x vel) elephant-speed)
		(setf (x vel) (- elephant-speed)))))
	 (setf k-vel vel)))) '(:stage)))

(let ((collision-rects (rect->collision-rects
			(centered-rect (scale-v elephant-dims 1/2) elephant-dims)
			6)))
  (elephant-methodf stage-collision (c stage)
    (astage-collisionsf
     (alist
      :left
      (collision-lambda
	(when (eq facing :left)
	  (setf facing :right)))
      :right
      (collision-lambda
	(when (eq facing :right)
	  (setf facing :left)))))))
