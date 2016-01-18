;;;; cave-story.lisp

(in-package #:cave-story)

#+nil
(progn
  (ql:quickload :cave-story)
  (in-package :cave-story)
  (swank:set-default-directory "/home/chebert/Projects/lisp/cave-story-lisp"))

(defvar *window*)
(defvar *renderer*)
(defvar *font*)
(defvar *global-game*)

;; Debug params.
(defparameter *update-period* 1
  "Number of frames per update. 1 for real-time.")

(defparameter *debug-input-keybindings*
  `((((:key :capslock :escape)) .
     ,(lambda () (quit)))
    (((:key :p)
      (:joy :start)) .
     ,(lambda () (setq *global-paused?* (not *global-paused?*))))
    (((:key :r)
      (:joy :select)) .
     ,(lambda () (setq *global-game* (reset))))
    (((:joy :r)) .
     ,(lambda ()
	      (case *input-playback*
		(:recording (begin-input-playback))
		(:playback (setq *input-playback* nil))
		(t (begin-input-recording)))))
    (((:key :n)) .
     ,(lambda ()
	      (when *global-paused?*
		(setq *global-game*
		      (update-and-render *global-game*)))))))

(defstruct game
  player
  camera
  stage
  projectile-groups
  damage-numbers
  active-systems

  (input (make-input)))

(let (debug-toggle-off?)
  (defun handle-debug-input (transient-input)
    (let ((pressed-keys (ti-pressed-keys transient-input))
	  (pressed-joy (ti-pressed-joy-buttons transient-input)))
      (loop for (bindings . action) in *debug-input-keybindings*
	 do (loop for (type . b) in bindings
	       do (when (ecase type
			  (:key (any? (intersection pressed-keys
						    b
						    :test #'eq)))
			  (:joy (any? (intersection pressed-joy
						    (mapcar #'joy-key->num b)
						    :test #'eq))))
		    (funcall action)
		    (return))))

      (when (find :f1 pressed-keys)
	(mapc (if debug-toggle-off?
		  (progn
		    (setq debug-toggle-off? nil)
		    #'remove-visible-layer!)
		  #'toggle-visible-layer!)
	      *debug-layers*))

      (loop for key in '(:f2 :f3 :f4 :f5 :f6)
	 for i from 0
	 do
	   (when (find key pressed-keys)
	     (let ((layer (elt *debug-layers* (1+ i))))
	       (print layer)
	       (setq debug-toggle-off? t)
	       (toggle-visible-layer! layer)))))))

(defun main ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       last-update-time)
	   (setq *global-game* (init))
	   (setq last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (let ((transient-input (gather-transient-input!)))
		    (handle-debug-input transient-input)
		    (setq *global-game*
			  (make-game :player (game-player *global-game*)
				     :camera (game-camera *global-game*)
				     :stage (game-stage *global-game*)
				     :projectile-groups (game-projectile-groups *global-game*)
				     :damage-numbers (game-damage-numbers *global-game*)
				     :active-systems (game-active-systems *global-game*)
				     :input (gather-input (game-input *global-game*)
							  transient-input)))))
		
		(when (>= frame-timer (* *update-period* *frame-time*))
		  (if *global-paused?*
		      (draw-text-line! (zero-v) "PAUSED")
		      (setq *global-game* (update-and-render *global-game*)))
		  (render! *renderer*
			   *font*
			   *render-list*
			   (camera-pos
			    (estate (game-camera *global-game*))
			    (stage-dims->camera-bounds (stage-dims (game-stage *global-game*)))))
		  (setq frame-timer (- frame-timer
				       (* *update-period* *frame-time*))))

		(let ((dt (- (sdl:get-ticks) last-update-time)))
		  ;; NOTE: if we are paused beyond our control, Don't play catchup.
		  (setq frame-timer
			(+ frame-timer
			   (min dt (* 2 *frame-time*)))))
		(setq last-update-time (sdl:get-ticks))
		(music-update)
		(sdl:delay 1)))
      (cleanup))))

(defun handle-input (game)
  "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."

  (let ((input (game-input game))
	(input-systems
	 (active-systems-input (estate (game-active-systems game)))))
    (update-input-subsystem input-systems input)
    (ecase (first input-systems)
      (:game
       (when (or (joy-pressed? input :b) (key-pressed? input :x))
	 ;; Fire Gun
	 (player-fire-gun! (estate (game-player game)))))

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

(defparameter *dorito-friction-acc* 0.00002)
(defparameter *dorito-bounce-speed* 0.225)

(defstruct pickup type amt)

(defun dorito-fns-alist ()
  (alist :ai-fn #'dorito-ai
	 :draw-fn #'dorito-drawing
	 :stage-collision-fn #'dorito-stage-collision
	 :pickup-rect-fn #'dorito-pickup-rect
	 :pickup-kill-fn #'dorito-pickup-kill
	 :pickup-data-fn #'dorito-pickup-data))

(defun make-default-dorito (pos vel size)
  (append
   (alist :timers
	  (alist :life
		 (create-expiring-timer (s->ms 8)
					t)
		 :anim-cycle
		 (create-timed-cycle 14
				     (alexandria.0.dev:iota
				      6)))
	  :physics
	  (alist :stage
		 (make-kin-2d :pos (-v pos (rect-pos
					    (dorito-collision-rect size)))
			      :vel vel
			      :accelerator-x
			      (friction-accelerator *dorito-friction-acc*)
			      :accelerator-y
			      (const-accelerator *gravity-acc*)
			      :clamper-vy
			      (clamper+- *terminal-speed*)))
	  :size size)
   (dorito-fns-alist)))

(def-entity-constructor create-dorito #'make-default-dorito
  :timers :physics :stage-collision :drawable :pickup)

(defun dorito-ai (d ticks)
  (declare (ignore ticks))
  (aset d :dead? (not (timer-active? (aval (aval d :timers) :life)))))

(defun dorito-pos (d)
  (physics-pos d))

(defun flash-time? (tr)
  (and (timer-active? tr) (zerop (chunk-timer-period tr 50))))

(defun death-flash? (timers)
  (let ((tr (aval timers :life)))
    (and (< (timer-ms-remaining tr) (s->ms 1))
	 (flash-time? tr))))

(defun dorito-drawing (d)
  (unless (death-flash? (aval d :timers))
    (make-sprite-drawing
     :layer :pickup
     :sheet-key :npc-sym

     :src-rect
     (create-rect
      (+v
       (anim-cycle-offset (aval d :timers))
       (tile-v 0 (1+ (position (aval d :size) '(:small :medium :large)))))
      (make-v (tiles 1) (1- (tiles 1))))

     :pos (physics-pos d))))

(defun set-x-v (v x)
  (make-v x (y v)))

(defun set-y-v (v y)
  (make-v (x v) y))

(defun reverse-x-v (v)
  (make-v (- (x v)) (y v)))

(defun max-y-v (v max-y)
  (make-v (x v) (max (y v) max-y)))

(defun dorito-stage-collision (d stage)
  (let* ((collision-rects (rect->collision-rects
			   (dorito-collision-rect (aval d :size))))
	 (physics (aval d :physics))
	 (stage-physics (aval physics :stage))
	 (data (alist :pos (aval stage-physics :pos)
		      :vel (aval stage-physics :vel)))
	 (res
	  (stage-collisions
	   data
	   stage
	   collision-rects
	   (alist
	    :bottom
	    (collision-lambda (data)
	      (push-sound :dorito-bounce)
	      (aset data
		    :vel
		    (set-y-v (aval data :vel)
			     (- *dorito-bounce-speed*))))
	    :right
	    (collision-lambda (data)
	      (if (plusp (x (aval data :vel)))
		  (aset data :vel (reverse-x-v (aval data :vel)))
		  data))
	    :left
	    (collision-lambda (data)
	      (if (minusp (x (aval data :vel)))
		  (aset data :vel (reverse-x-v (aval data :vel)))
		  data))
	    :top
	    (collision-lambda (data)
	      (aset data :vel (max-y-v (aval data :vel) 0)))))))
    (aset d :physics
	  (aset physics
		:stage
		(aset stage-physics
		      :pos (aval res :pos)
		      :vel (aval res :vel))))))

(defun dorito-pickup-rect (d)
  (rect-offset (dorito-collision-rect (aval d :size)) (physics-pos d)))

(defun dorito-pickup-kill (d)
  (push-sound :pickup)
  (aset d :dead? t))

(defun dorito-pickup-data (d)
  (make-pickup :type :dorito
	       :amt (ecase (aval d :size)
		      (:small 1)
		      (:medium 10)
		      (:large 20))))

(defun dorito-draw (life-tr anim-cycle-current size pos)
  (unless (and (< (timer-ms-remaining life-tr) (s->ms 1))
	       (zerop (chunk-timer-period life-tr 50)))
    (draw-sprite!
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

(defun single-loop-sprite-fns-alist ()
  (alist :ai-fn #'single-loop-sprite-ai))

(defun make-default-single-loop-sprite (fps seq sheet-key tile-y layer)
  (amerge
   (single-loop-sprite-fns-alist)
   (alist :timers (alist :cycle (create-timed-cycle fps seq))
	  :sheet-key sheet-key
	  :layer layer
	  :tile-y tile-y)))

(def-entity-constructor create-single-loop-sprite #'make-default-single-loop-sprite
  :timers)

(defun single-loop-sprite-drawing (s pos)
  (unless (aval s :dead?)
    (make-sprite-drawing :layer (aval s :layer)
			 :sheet-key (aval s :sheet-key)
			 :src-rect
			 (tile-rect (tile-v (cycle-current
					     (timed-cycle-cycle
					      (aval (aval s :timers) :cycle)))
					    (aval s :tile-y)))
			 :pos pos)))

(defun single-loop-sprite-ai (p ticks)
  (let ((dead? (and (find :cycle ticks)
		    (zerop (cycle-idx
			    (timed-cycle-cycle
			     (aval (aval p :timers) :cycle)))))))
    (aset p :dead? dead?)))


(defun particle-fns-alist ()
  (alist :draw-fn #'particle-drawing
	 :ai-fn
	 (lambda (p ticks)
	   (declare (ignore ticks))
	   (aset p :dead? (aval (estate (aval p :single-loop-sprite)) :dead?)))))

(defun make-default-particle (&key seq fps sheet-key tile-y pos)
  (amerge
   (particle-fns-alist)
   (alist :single-loop-sprite
	  (create-single-loop-sprite fps seq
				     sheet-key
				     tile-y
				     :particle)
	  :pos pos)))

;; NOTE: This (ai/timers) is a kludge to make it so particle can set its
;; dead? flag based on single-loop-sprite.
(def-entity-constructor create-particle #'make-default-particle
  :drawable :timers :ai)

(defun particle-drawing (p)
  (let ((sp (estate (aval p :single-loop-sprite))))
    (if (aval sp :dead?)
	nil
	(let ((cycle-current (cycle-current
			      (timed-cycle-cycle (aval (aval sp :timers) :cycle)))))
	  (make-sprite-drawing :layer :particle
			       :sheet-key (aval sp :sheet-key)
			       :src-rect
			       (tile-rect (tile-v cycle-current (aval sp :tile-y)))
			       :pos (aval p :pos))))))

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

(defun number-drawing
    (pos number &key (layer :hud) (centered? t) (show-sign? t))
  (let* ((neg? (minusp number))
	 (digits (fixnum->digits number))
	 (pos (if centered?
		  (sub-v pos (tiles/2-v (/ (length digits) 2) 1/2))
		  pos))
	 (drawings nil))

    (dolist (digit digits)
      (when (or show-sign? (numberp digit))
	(let ((src-pos
	       (case digit
		 (:positive (tiles/2-v 4 6))
		 (:negative (tiles/2-v 5 6))
		 (t (tiles/2-v digit (if neg? 8 7))))))
	  (push (make-sprite-drawing
		 :layer layer
		 :sheet-key :text-box
		 :src-rect (create-rect src-pos (tile-dims/2))
		 :pos pos)
		drawings))
	(setq pos (+v pos (tiles/2-v 1 0)))))
    drawings))

(defun fixnum->digits (number)
  (if (zerop number)
      (list 0)
      (let (digits
	    neg?)
	(when (minusp number)
	  (setq neg? t)
	  (setq number (* -1 number)))
	(loop while (plusp number) do
	     (multiple-value-bind (num rem) (floor number 10)
	       (push rem digits)
	       (setq number num)))
	(if neg?
	    (cons :negative digits)
	    (cons :positive digits)))))


(defun floating-number-fns-alist ()
  (alist :draw-fn #'floating-number-drawing
	 :ai-fn #'floating-number-ai))

(defun make-default-floating-number (entity amt)
  (amerge
   (floating-number-fns-alist)
   (alist :entity entity
	  :amt amt
	  :timers
	  (alist
	   :life (create-expiring-timer (s->ms 2) t))
	  :physics
	  (alist
	   :offset (make-offset-motion (zero-v)
				       :up
				       (/ (tiles 1/30) *frame-time*))))))

(def-entity-constructor create-floating-number #'make-default-floating-number
  :timers :drawable :physics)

(defun floating-number-drawing (fn)
  (number-drawing (+v (origin (estate (aval fn :entity)))
		      (physics-pos fn))
		  (aval fn :amt)
		  :layer :floating-text))

(defun floating-number-ai (fn ticks)
  (declare (ignore ticks))
  (let ((dead? (not (timer-active? (aval (aval fn :timers) :life)))))
    (cond ((< (y (motion-pos (cdr (assoc :offset (aval fn :physics)))))
	      (- (tiles 1)))
	   (aset fn
		 :dead? dead?
		 :physics
		 (aset (aval fn :physics)
		       :offset
		       (make-offset-motion (zero-v :y (- (tiles 1))) :up 0))))
	  (t (aset fn :dead? dead?)))))

(defun floating-number-add-amt (fn amount)
  (aset fn
	:timers (aupdate (aval fn :timers) #'reset-timer :life)
	:amt (+ (aval fn :amt) amount)))

(defun remove-all-dead (game)
  (replace-entity-state (game-projectile-groups game)
			#'projectile-groups-remove-dead)
  (replace-entity-state (game-damage-numbers game)
			#'damage-numbers-remove-dead)
  (values))

(defun hud-number-drawing (tile/2-y number)
  (number-drawing (tiles/2-v (+ (if (< number 10) 1 0) 3)
			     tile/2-y)
		  number
		  :centered? nil
		  :show-sign? nil))

(defun exp-for-gun-level (gun-name lvl)
  (elt (cdr (assoc gun-name *gun-level-exps*))
       (if (eq lvl :max)
	   2
	   lvl)))

#+nil
(progn
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

(defparameter *text-speed* 100)
(defun slow-text-speed ()
  (setq *text-speed* 100))
(defun fast-text-speed ()
  (setq *text-speed* 25))
(defparameter *cursor-blink-time* 100)

#+nil
(defstruct (text-display (:include entity-state))
  pos
  text
  num-chars
  wait-for-input?
  blink-time)

#+nil
(defun make-default-text-display (pos text)
  (make-text-display :pos pos
		     :text text
		     :num-chars 0
		     :timers
		     (alist :text (create-expiring-timer *text-speed* t))
		     :wait-for-input? t
		     :blink-time 0))
#+nil
(def-entity-constructor create-text-display #'make-default-text-display
  :timers :drawable)

#+nil
(defun text-display-ai (td ticks)
  (cond
    ((= (text-display-num-chars td) (length (text-display-text td)))
     (when (and (text-display-wait-for-input? td)
		(> 2 (chunk-time-period (text-display-blink-time td)
					*cursor-blink-time*
					5)))
       (let ((char-dims (get-text-size *font* " "))
	     (w (x (get-text-size *font* (text-display-text td)))))
	 (draw-rect!
	  (create-rect (+v (text-display-pos td) (make-v w 0)) char-dims)
	  *white*
	  :layer :text
	  :filled? t)))
     (make-text-display
      :timers (text-display-timers td)
      :pos (text-display-pos td)
      :text (text-display-text td)
      :num-chars (text-display-num-chars td)
      :wait-for-input? (text-display-wait-for-input? td)
      :blink-time (+ (text-display-blink-time td) *frame-time*)
      :dead? (text-display-dead? td)))
    ((not (timer-active? (aval (text-display-timers td) :text)))
     (push-sound :text-click)
     (make-text-display
      :timers (aset (text-display-timers td) :text (create-expiring-timer *text-speed* t))
      :pos (text-display-pos td)
      :text (text-display-text td)
      :num-chars (+ 1 (text-display-num-chars td))
      :wait-for-input? (text-display-wait-for-input? td)
      :blink-time (+ (text-display-blink-time td) *frame-time*)
      :dead? (text-display-dead? td)))
    (t td)))

#+nil
(defmethod ai ((td text-display) ticks)
  (text-display-ai td ticks))

#+nil
(defun text-display-drawing (td)
  (list* (make-text-line-drawing
	  :pos (text-display-pos td)
	  :text (subseq (text-display-text td)
			0
			(text-display-num-chars td))
	  :layer :text)
	 (draw-textbox 5 21 30 8)))

#+nil
(defmethod draw ((td text-display))
  (text-display-drawing td))

(defun hud-fns-alist ()
  (alist :draw-fn #'hud-drawing))
(defun make-default-hud  (player gun-exps id)
  (values
   (amerge
    (hud-fns-alist)
    (alist :player player
	   :gun-exps gun-exps
	   :id id
	   :timers
	   (alist
	    :exp-change (create-expiring-timer (s->ms 1))
	    :health-change (create-expiring-timer (s->ms 1/2)))))
   id))

(def-entity-constructor create-hud #'make-default-hud
  :timers :drawable)

(defun hud-drawing (hud)
  (let ((bar-tile/2-w 5)
	(bar-tile/2-x 5)
	(drawings))
    (push
     (make-sprite-drawing :layer
			  :hud-bg :sheet-key :text-box
			  :src-rect
			  (create-rect-cmpts 0 (tiles/2 5) (tiles/2 8) (tiles/2 1))
			  :pos (tile-v 1 2))
     drawings)

    (let ((health (aval (player-state (aval hud :player)) :health-amt)))
      (when (timer-active? (aval (aval hud :timers) :health-change))
	(push
	 (make-sprite-drawing
	  :layer :hud
	  :sheet-key :text-box
	  :src-rect
	  (create-rect-cmpts 0 (tiles/2 4)
			     (floor (* (tiles/2 bar-tile/2-w)
				       (/ (aval hud :last-health-amt)
					  (aval (player-state (aval hud :player))
						:max-health-amt))))
			     (tiles/2 1))
	  :pos (tiles/2-v bar-tile/2-x 4))
	 drawings))
      (push
       (make-sprite-drawing
	:layer :hud-fg
	:sheet-key :text-box
	:src-rect
	(create-rect-cmpts 0 (tiles/2 3)
			   (floor (* (tiles/2 bar-tile/2-w)
				     (/ health
					(aval (player-state (aval hud :player))
					      :max-health-amt))))
			   (tiles/2 1))
	:pos (tiles/2-v bar-tile/2-x 4))
       drawings)
      (appendf drawings (hud-number-drawing 4 health)))

    (let ((exp-pos (tiles/2-v bar-tile/2-x 3)))
      (push
       (make-sprite-drawing :layer :hud-bg
			    :sheet-key :text-box
			    :src-rect
			    (create-rect-cmpts 0 (tiles/2 9) (tiles/2 5) (tiles/2 1))
			    :pos exp-pos)
       drawings)

      (when (flash-time? (aval (aval hud :timers) :exp-change))
	(push
	 (make-sprite-drawing :layer :hud-fg
			      :sheet-key :text-box
			      :src-rect
			      (create-rect (tiles/2-v 5 10)
					   (tiles/2-v bar-tile/2-w 1))
			      :pos exp-pos)
	 drawings))

      (multiple-value-bind (exp gun-name) (current-gun-exp (aval hud :player) (aval hud :gun-exps))
	(let* ((current-level (gun-level exp (cdr (assoc gun-name *gun-level-exps*))))
	       (next-lvl-exp (exp-for-gun-level gun-name current-level))
	       (current-lvl-exp (if (zerop current-level)
				    0
				    (exp-for-gun-level gun-name (1- current-level)))))
	  (if (= exp (exp-for-gun-level gun-name :max))
	      (push
	       (make-sprite-drawing :layer :hud-fg :sheet-key :text-box
				    :src-rect
				    (create-rect (tiles/2-v 5 9)
						 (tiles/2-v bar-tile/2-w 1))
				    :pos exp-pos)
	       drawings)

	      (push
	       (make-sprite-drawing
		:layer :hud
		:sheet-key :text-box
		:src-rect
		(create-rect-cmpts 0 (tiles/2 10)
				   (floor (* (tiles/2 bar-tile/2-w)
					     (/ (- exp current-lvl-exp)
						(- next-lvl-exp current-lvl-exp))))
				   (tiles/2 1))
		:pos exp-pos)
	       drawings))

	  (push
	   (make-sprite-drawing
	    :layer :hud
	    :sheet-key :text-box
	    :src-rect
	    (create-rect-cmpts (tiles/2 10) (tiles/2 10)
			       (tiles/2 2) (tiles/2 1))
	    :pos (make-v (tiles/2 2) (y exp-pos)))
	   drawings)

	  (appendf drawings (hud-number-drawing 3 (1+ current-level))))))
    drawings))

(defun hud-exp-changed (hud)
  (aset hud
	:timers (aupdate (aval hud :timers) #'reset-timer :exp-change)))

(defun hud-health-changed (hud)
  (aset hud
	:timers (aupdate (aval hud :timers) #'reset-timer :health-change)
	:last-health-amt (aval (player-state (aval hud :player)) :health-amt)))

(defun textbox-tile-drawing (src-pos pos)
  (let ((size (both-v (tiles/2 1))))
    (make-sprite-drawing :layer :text-box :sheet-key :text-box
			 :src-rect
			 (create-rect src-pos size)
			 :pos pos)))

(defun draw-textbox (text-x text-y text-width text-height)
  (let ((right 30)
	(left 0)
	(top 0)
	(mid 1)
	(bottom 2)
	(top-y text-y)
	(bottom-y (+ text-y text-height -1))
	(drawings nil))

    (push (textbox-tile-drawing (tiles/2-v left top) (tiles/2-v text-x top-y)) drawings)
    (push (textbox-tile-drawing (tiles/2-v left bottom) (tiles/2-v text-x bottom-y)) drawings)

    (dotimes (i (- text-height 2))
      (push (textbox-tile-drawing (tiles/2-v left mid) (tiles/2-v text-x (+ i text-y 1)))
	    drawings))

    (dotimes (i (- text-width 2))
      (let ((x (+ 1 i text-x)))
	(dotimes (j (- text-height 2))
	  (push (textbox-tile-drawing (tiles/2-v mid mid) (tiles/2-v x (+ j text-y 1)))
		drawings))
	(push (textbox-tile-drawing (tiles/2-v mid top) (tiles/2-v x top-y))
	      drawings)
	(push (textbox-tile-drawing (tiles/2-v mid bottom) (tiles/2-v x bottom-y))
	      drawings)))

    (let ((x (+ -1 text-width text-x)))
      (push (textbox-tile-drawing (tiles/2-v right top) (tiles/2-v x top-y))
	    drawings)
      (dotimes (i (- text-height 2))
	(push (textbox-tile-drawing (tiles/2-v right mid) (tiles/2-v x (+ 1 text-y i)))
	      drawings))
      (push (textbox-tile-drawing (tiles/2-v right bottom) (tiles/2-v x bottom-y))
	    drawings))
    drawings))

(defun update-and-render (game)
  "The Main Loop, called once per *FRAME-TIME*."
  (when (eq *input-playback* :playback)
    (setq game
	  (make-game :player (game-player game)
		     :camera (game-camera game)
		     :stage (game-stage game)
		     :projectile-groups (game-projectile-groups game)
		     :damage-numbers (game-damage-numbers game)
		     :active-systems (game-active-systems game)
		     :input (next-playback-input))))
  (handle-input game)

  (setq *render-list* nil)

  (case *input-playback*
    (:recording
     (draw-text-line! (zero-v) "RECORD"))
    (:playback
     (draw-text-line! (zero-v) "PLAYBACK")))

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

  ;; (draw-point! (player-nozzle-pos player) *red*)
  (let ((focus (camera-focus (estate (game-camera game))))
	(camera-bounds (stage-dims->camera-bounds (stage-dims (game-stage game)))))
    (draw-point! focus *cyan*)
    (draw-point! (clamp-pos focus camera-bounds) *red*)
    (draw-rect! camera-bounds *cyan*))
  (draw-point! (camera-target-from-player (player-state (game-player game))) *white*)
  ;; End Debug Drawings.

  (play-sounds *sfx-play-list*)
  (setq *sfx-play-list* nil)

  (when (eq *input-playback* :recording)
    (record-frame-input (game-input game)))

  (make-game :player (game-player game)
	     :camera (game-camera game)
	     :stage (game-stage game)
	     :projectile-groups (game-projectile-groups game)
	     :damage-numbers (game-damage-numbers game)
	     :active-systems (game-active-systems game)
	     :input (reset-transient-input (game-input game))))

(defparameter *input-playback* nil)

(let (inputs playback-idx game-state)
  (defun record-frame-input (input)
    (push input inputs))

  (defun next-playback-input ()
    (setq playback-idx (- playback-idx 1))
    (when (= 0 playback-idx)
      (restore-state game-state)
      (setq playback-idx (1- (length inputs))))
    (elt inputs playback-idx))

  (defun end-input-playback ()
    (setq *input-playback* nil))

  (defun begin-input-playback ()
    (restore-state game-state)
    (setq playback-idx (1- (length inputs)))
    (setq *input-playback* :playback))

  (defun begin-input-recording ()
    (setq *input-playback* :recording)
    (setq inputs nil)
    (setq game-state (save-current-state))))

(defparameter *dialog-text-pos* (tiles/2-v 7 24))
(defvar *global-paused?*)
(defparameter *entity-systems* '(:game :dialog))

(defstruct active-systems
  (update (list :game))
  (draw (list :game))
  (input (list :game)))

(defun active-systems-switch-to-dialog ()
  (make-active-systems
   :update (list :dialog)
   :draw (list :game :dialog)
   :input (list :dialog)))

(defun create-active-systems (&key (id (gen-entity-id)))
  (create-entity (make-active-systems) () :id id))

(defun damage-numbers-remove-dead (d)
  (remove-if (lambda (pair) (aval (estate (cdr pair)) :dead?)) d))

(defun damage-numbers-update-damage-amt (d e amt)
  (let ((existing-dn-pair (assoc e d)))
    (if existing-dn-pair
	(replace-entity-state (cdr existing-dn-pair)
			      (lambda (fn)
				(floating-number-add-amt fn (- amt))))
	(push (cons e (create-floating-number e (- amt))) d)))
  d)

(defun create-damage-numbers (&key (id (gen-entity-id)))
  (create-entity nil () :id id))

(defun gun-exp-for (gun-exps gun-name)
  (cdr (assoc gun-name gun-exps)))

(defun incr-gun-exp (gun-exps gun-name amt)
  (aupdate gun-exps
	   (lambda (gun-amt)
	     (min (max (+ gun-amt amt) 0)
		  (exp-for-gun-level gun-name :max)))
	   gun-name))

(defun create-gun-exps (&key (id (gen-entity-id)))
  (create-entity
   (loop for g across *gun-names* collecting (cons g 0))
   ()
   :id id))

(defun projectile-groups-remove-dead (g)
  (remove-if #'null
	     (loop for (name . g) in g
		collecting
		  (let ((new-g (remove-if
				(lambda (x)
				  (dead? (estate x))) g)))
		    (when new-g
		      (list* name new-g))))))

(defun projectile-groups-count (g gun-name)
  (count gun-name g :key #'car))

(defun projectile-groups-add (g pg)
  (push pg g))

(defun create-projectile-groups (&key (id (gen-entity-id)))
  (create-entity nil () :id id))

(defun update-damage-number-amt (damage-numbers e amt)
  (replace-entity-state damage-numbers
			(lambda (dn)
			  (damage-numbers-update-damage-amt dn e amt))))

(defun current-gun-exp (player gun-exps)
  (let* ((gun-name (player-current-gun-name (player-state player)))
	 (exp (gun-exp-for (estate gun-exps) gun-name)))
    (values
     exp
     gun-name)))

(defun save-current-state ()
  (list (current-entity-states) (copy-game *global-game*)))

(defun restore-state (state)
  (dolist (s *registry-syms*)
    (set s nil))
  (restore-entity-states (first state))
  (setq *global-game* (second state)))

(defun create-game ()
  (let ((damage-numbers (create-damage-numbers))
	(projectile-groups (create-projectile-groups))
	(stage (basic-stage))
	(hud (gen-entity-id))
	(gun-exps (create-gun-exps))
	(active-systems (create-active-systems)))
    (create-stage! stage)
    (let* ((player (create-default-player hud
					  projectile-groups
					  damage-numbers
					  gun-exps
					  active-systems))
	   (camera (create-player-camera (v/2 *window-dims*) (zero-v) player)))
      (create-hud player gun-exps hud)

      (create-critter (make-v (+ (tiles 14) (tiles 1/4))
			      (tiles 6))
		      player
		      damage-numbers)
      (create-elephant (make-v (tiles 7) (tiles 6)) player camera damage-numbers)
      (dolist (x '(1 3 6 7))
	(create-bat x 7 player))
      (create-dorito (make-v (+ (tiles 14) (tiles 1/4))
			     (tiles 6))
		     (make-v 0 0)
		     :medium)
      (make-game :player player
		 :camera camera
		 :stage stage
		 :projectile-groups projectile-groups
		 :active-systems active-systems
		 :damage-numbers damage-numbers))))

(defun reset ()
  (switch-to-new-song :lastcave)
  (set-music-volume 20)

  (dolist (s *registry-syms*)
    (set s nil))
  (init-entity-registry)

  (setq *global-paused?* nil)

  (create-game))

(defun init ()
  "Called at application startup."
  (sdl:init '(:audio :video :joystick))
  (sdl.ttf:init)
  (setq *font* (sdl.ttf:open-font "./content/VeraMoBd.ttf" 19))
  (sdl.mixer:open-audio sdl.mixer:+default-frequency+
			sdl.mixer:+default-format+
			2
			4096)

  (init-input!)
  (sdl:show-cursor :disable)

  (put-all-resources)
  (setq *current-song* nil)

  (multiple-value-setq
      (*window* *renderer*)
    (sdl:default-window-and-renderer
	"Cave Story"
	(x *window-dims*) (y *window-dims*)
	0))
  (reset))

(defun cleanup ()
  "Called at application closing to cleanup all subsystems."
  (cleanup-input!)
  (cleanup-all-resources)
  (clrhash *character-textures*)

  (setq *input-playback* nil)
  (sdl.mixer:close-audio)
  (sdl:destroy-renderer *renderer*)
  (sdl:destroy-window *window*)
  (sdl.ttf:quit)
  (sdl:quit)
  (setq *renderer* nil
	*window* nil))

(defun quit ()
  "Quits the application."
  (throw 'exit nil))

(defun face-player (pos player)
  (if (< (x pos) (x (physics-pos (player-state player)))) :right :left))

(defun bat-fns-alist ()
  (alist :damageable-hit-react-fn #'bat-hit-react
	 :origin-fn #'physics-tile-origin
	 :draw-fn #'bat-drawing
	 :ai-fn #'bat-ai
	 :damage-collision-rect-fn
	 (compose #'point-rect #'origin)
	 :damage-collision-amt-fn (constantly 1)
	 :damageable-rect-fn #'physics-tile-rect))

(defun make-default-bat (tile-x tile-y player)
  (amerge
   (bat-fns-alist)
   (alist :physics
	  (alist
	   :wave (make-wave-motion
		  :origin (tile-v tile-x
				  tile-y)
		  :dir :up
		  :amp (tiles 2)
		  :speed (/ 0.0325
			    *frame-time*)))
	  :timers
	  (alist
	   :anim-cycle (create-timed-cycle 14 #(0 2 1 2)))
	  :health-amt 1
	  :player player)))

(def-entity-constructor create-bat #'make-default-bat
  :timers :damage-collision :damageable :drawable
  :physics)

(defun bat-hit-react (b amt)
  (let ((physics (aset (aval b :physics)
		       :shake
		       (make-wave-motion :dir :left :amp 2 :speed 0.1 :rads 0)))
	(timers (aset (aval b :timers)
		      :shake (create-expiring-timer (s->ms 1/3) t)))
	(damage-numbers (aval b :damage-numbers))
	(id (aval b :id))
	(health-amt (aval b :health-amt)))
    (cond ((< amt health-amt)
	   (update-damage-number-amt damage-numbers id amt)
	   (push-sound :enemy-hurt)
	   (aset b
		 :physics physics
		 :timers timers
		 :health-amt (- health-amt amt)))
	  (t
	   (let ((origin (origin b)))
	     (push-sound :enemy-explode)
	     (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	     (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	     (create-death-cloud-particles 3 origin)

	     (aset b :dead? t))))))

(defun bat-drawing (b)
  (make-sprite-drawing :layer :enemy
		       :sheet-key :npc-cemet
		       :src-rect
		       (tile-rect (+v (tile-v 2 2)
				      (anim-cycle-offset (aval b :timers))
				      (facing-offset (aval b :facing))))
		       :pos (physics-pos b)))

(defun bat-ai (b ticks)
  (declare (ignore ticks))
  (aset b :facing (face-player (physics-pos b) (aval b :player))))

(defun facing-offset (facing)
  (tile-v 0 (if (eq facing :left) 0 1)))

(defun anim-cycle-offset (timers)
  (tile-v (timed-cycle-current (aval timers :anim-cycle)) 0))

(defun anim-cycle-val (timers)
  (timed-cycle-current (aval timers :anim-cycle)))

(defun point-rect (pos)
  (create-rect pos (both-v 1)))

(defun polar-vec->v (angle mag)
  (make-v (* mag (cos angle))
	  (* mag (sin angle))))

(defun death-cloud-particle-fns-alist ()
  (alist :draw-fn #'death-cloud-particle-drawing
	 :stage-collision-fn #'death-cloud-particle-stage-collision
	 :ai-fn (lambda (p ticks)
		  (declare (ignore ticks))
		  (aset p
			:dead?
			(aval (estate (aval p :single-loop-sprite))
			      :dead?)))))

(defun make-default-death-cloud-particle (pos)
  (amerge
   (death-cloud-particle-fns-alist)
   (alist :single-loop-sprite
	  (create-single-loop-sprite
	   15 (mapcar #'1+ (alexandria.0.dev:iota 7))
	   :npc-sym 0 :particle)
	  :physics
	  (alist
	   :stage
	   (make-kin-2d
	    :pos (-v pos (tile-dims/2))
	    :vel (polar-vec->v (rand-angle)
			       (rand-val-between 0.1 0.3))
	    :clamper-vx
	    (clamper+- *terminal-speed*)
	    :clamper-vy
	    (clamper+- *terminal-speed*))))))

(def-entity-constructor create-death-cloud-particle #'make-default-death-cloud-particle
  :drawable :physics :stage-collision :timers :ai)

(defun death-cloud-particle-drawing (d)
  (single-loop-sprite-drawing (estate (aval d :single-loop-sprite))
			      (physics-pos d)))


(let ((collision-rects (rect->collision-rects
			(centered-rect (tile-dims/2) (both-v (tiles 2/5))))))
  (defun death-cloud-particle-stage-collision (d stage)
    (let* ((physics (aval d :physics))
	   (stage-physics (aval physics :stage))
	   (data (alist :pos (aval stage-physics :pos)
			:vel (aval stage-physics :vel)))
	   (res
	    (stage-collisions
	     data stage collision-rects
	     (let ((stop-x
		    (collision-lambda (data)
		      (aset data
			    :vel
			    (set-x-v (aval data :vel) 0))))
		   (stop-y
		    (collision-lambda (data)
		      (aset data
			    :vel
			    (set-y-v (aval data :vel) 0)))))
	       (alist :bottom stop-y :left stop-x
		      :right stop-x :top stop-y)))))

      (aset d
	    :physics (aset physics
			   :stage
			   (aset stage-physics
				 :pos (aval res :pos)
				 :vel (aval res :vel)))))))



(defun create-death-cloud-particles (num pos)
  (dotimes (i num)
    (create-death-cloud-particle pos))
  (values))

(defparameter *critter-dynamic-collision-rect*
  (make-rect :pos (tile-v 0 1/4) :size (tile-v 1 3/4)))

(defun gravity-kin-2d (&key (pos (zero-v)) (vel (zero-v)))
  (make-kin-2d :pos pos
	       :vel vel
	       :accelerator-y (const-accelerator *gravity-acc*)
	       :clamper-vy (clamper+- *terminal-speed*)))

(defun critter-fns-alist ()
  (alist :ai-fn #'critter-ai
	 :draw-fn #'critter-drawing
	 :damageable-rect-fn #'physics-tile-rect
	 :damageable-hit-react-fn #'critter-hit-react
	 :damage-collision-rect-fn #'physics-tile-rect
	 :damage-collision-amt-fn (constantly 1)
	 :dynamic-collision-rect-fn #'critter-damage-collision-rect
	 :dynamic-collision-react-fn
	 (lambda (c side player-collision-rect player)
	   (dynamic-collision-enemy-react (physics-pos c) (origin c) (aval c :id)
					  (dynamic-collision-rect c) side
					  player-collision-rect (estate player)))
	 :stage-collision-fn #'critter-stage-collision
	 :origin-fn #'physics-tile-origin
	 :inertia-vel-fn #'critter-inertia-vel))

(defun make-default-critter (pos player damage-numbers)
  (let ((id (gen-entity-id)))
    (values
     (aset
      (critter-fns-alist)
      :physics (alist :stage (gravity-kin-2d :pos pos))
      :player player
      :health-amt 2
      :damage-numbers damage-numbers
      :id id)
     id)))

(def-entity-constructor create-critter #'make-default-critter
  :timers :drawable :physics :damageable
  :damage-collision :dynamic-collision :stage-collision)

(defun origin-dist (a b)
  (dist (origin a) (origin b)))

(defun critter-ai (c ticks)
  (declare (ignore ticks))
  (let ((physics (aval c :physics))
	(timers (aval c :timers))
	(facing (aval c :facing)))
    (unless (timer-active? (aval timers :shake))
      (setq physics (arem physics :shake)))

    (when (and (not (timer-active? (aval timers :sleep)))
	       (< (origin-dist c (estate (aval c :player))) (tiles 4)))
      (when (aval c :ground-tile)
	(setq physics
	      (aupdate physics
		       (lambda (kin-2d)
			 (aset kin-2d
			       :vel
			       (make-v (* 0.04 (if (eq facing :left) -1 1))
				       (- 0.35))))
		       :stage))))
    (aset c :physics physics
	  :facing (face-player (physics-pos c) (aval c :player)))))

(defun critter-drawing (c)
  (let* ((sleep-timer (aval (aval c :timers) :sleep))
	 (sprite-tile-x (cond
			  ((and sleep-timer (timer-active? sleep-timer))
			   0)
			  ((< (origin-dist c (estate (aval c :player))) (tiles 7))
			   1)
			  (t
			   0))))
    (make-sprite-drawing :layer :enemy
			 :sheet-key :npc-cemet
			 :src-rect (tile-rect (+v (tile-v sprite-tile-x 0)
						  (facing-offset (aval c :facing))))
			 :pos (physics-pos c))))

(defun critter-hit-react (c amt)
  (let ((health-amt (aval c :health-amt)))
    (if (< amt health-amt)
	(progn
	  (update-damage-number-amt (aval c :damage-numbers) (aval c :id) amt)
	  (push-sound :enemy-hurt)

	  (aset c
		:physics
		(aset (aval c :physics)
		      :shake
		      (make-wave-motion :dir :left :amp 2 :speed 0.1 :rads 0))
		:timers
		(aset (aval c :timers) :shake (create-expiring-timer (s->ms 1/3) t))
		:health-amt (- health-amt amt)))
	(let ((origin (origin c)))
	  (push-sound :enemy-explode)
	  (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	  (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	  (create-death-cloud-particles 6 origin)
	  (aset c :dead? t)))))

(defun critter-damage-collision-rect (c)
  (rect-offset *critter-dynamic-collision-rect* (physics-pos c)))

(defun dynamic-collision-enemy-react
    (pos origin id dynamic-collision-rect side player-collision-rect player-state)
  (let* ((physics (aval player-state :physics))
	 (kin-2d (aval physics :stage))
	 (ground-tile (aval player-state :ground-tile))
	 (ground-inertia-entity (aval player-state :ground-inertia-entity)))
    (setq player-state
	  (aset player-state
		:physics
		(aset
		 physics
		 :stage
		 (let ((player-rect (rect-offset player-collision-rect
						 (aval kin-2d :pos))))
		   (case side
		     (:bottom
		      (cond
			((and (not (player-on-ground? player-state))
			      (<= (y pos) (bottom player-rect) (+ (y origin))))
			 (setq ground-tile :dynamic
			       ground-inertia-entity id)
			 (aset
			  kin-2d
			  :vel (zero-v :x (x (aval kin-2d :vel)))
			  :pos (-v
				(flush-rect-pos player-rect
						(y (rect-pos dynamic-collision-rect))
						:up)
				(rect-pos player-collision-rect))))
			(t kin-2d)))
		     ((:left :right)
		      (let ((disp (- (x (aval kin-2d :pos)) (x pos))))
			(cond ((> (abs disp) (tiles 1/4))
			       (aset
				kin-2d
				(make-v (* (/ *terminal-speed* 70) disp) (y (aval kin-2d :vel)))
				:vel))
			      (t kin-2d))))
		     (t kin-2d))))))
    (aset player-state
	  :ground-tile ground-tile
	  :ground-inertia-entity ground-inertia-entity)))

(let ((collision-rects (rect->collision-rects
			(centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6)))
  (defun critter-stage-collision (c stage)
    (let* ((physics (aval c :physics))
	   (last-tile (aval c :ground-tile))
	   (stage-physics (aval physics :stage))
	   (data (alist :pos (aval stage-physics :pos)
			:vel (aval stage-physics :vel)
			:ground-tile nil
			:last-ground-tile last-tile
			:timers (aval c :timers)))
	   (res
	    (stage-collisions
	     data stage collision-rects
	     (alist
	      :bottom
	      (collision-lambda (data)
		(amerge
		 (unless last-tile
		   (alist :timers
			  (aset (aval data :timers)
				:sleep
				(create-expiring-timer (s->ms 1/3) t))))
		 (alist :ground-tile (aval data :tile-type)
			:vel (zero-v))
		 data))
	      :top
	      (collision-lambda (data)
		(aset data :vel (max-y-v (aval data :vel) 0)))))))
      (aset c
	    :physics (aset physics
			   :stage
			   (aset stage-physics
				 :pos (aval res :pos)
				 :vel (aval res :vel)))
	    :timers (aval res :timers)
	    :ground-tile (aval res :ground-tile)))))

(defun physics-tile-origin (c)
  (+v (physics-pos c) (tile-dims/2)))
(defun physics-tile-rect (c)
  (tile-rect (physics-pos c)))

(defun stage-vel (physics)
  (aval (aval physics :stage) :vel))

(defun critter-inertia-vel (c)
  (stage-vel (aval c :physics)))

(defparameter *elephant-speed* 0.08)

(defun elephant-fns-alist ()
  (alist :draw-fn #'elephant-drawing
	 :origin-fn #'elephant-origin
	 :inertia-vel-fn #'elephant-inertia-vel
	 :damageable-rect-fn #'elephant-damageable-rect
	 :damageable-hit-react-fn #'elephant-hit-react
	 :dynamic-collision-rect-fn #'elephant-dynamic-collision-rect
	 :damage-collision-amt-fn #'elephant-damage-collision-amt
	 :damage-collision-rect-fn #'elephant-damage-collision-rect
	 :dynamic-collision-react-fn
	 (lambda (c side player-collision-rect player)
	   (dynamic-collision-enemy-react (physics-pos c)
					  (origin c)
					  (aval c :id)
					  (dynamic-collision-rect c)
					  side
					  player-collision-rect
					  (estate player)))
	 :ai-fn #'elephant-ai
	 :stage-collision-fn #'elephant-stage-collision))

(defun make-default-elephant  (pos player camera damage-numbers)
  (let ((id (gen-entity-id)))
    (values
     (amerge
      (elephant-fns-alist)
      (alist :physics
	     (alist
	      :stage
	      (gravity-kin-2d :pos pos
			      :vel (make-v (- *elephant-speed*) 0)))
	     :timers
	     (alist
	      :anim-cycle (create-timed-cycle 12 #(0 2 4)))
	     :health-amt 8
	     :facing :left
	     :damage-numbers damage-numbers
	     :camera camera
	     :player player
	     :id id))
     id)))

(def-entity-constructor create-elephant #'make-default-elephant
  :timers :drawable :physics :stage-collision
  :damageable :damage-collision :dynamic-collision)

(defparameter *elephant-dims* (make-v (tiles 2) (tiles 3/2)))
(defun elephant-drawing (e)
  (let ((src-pos
	 (cond
	   ((timer-active? (aval (aval e :timers) :recover))
	    (make-v (tiles (* 2 3)) 0))
	   (t (anim-cycle-offset (aval e :timers))))))
    (make-sprite-drawing :layer :enemy
			 :sheet-key :npc-eggs1
			 :src-rect
			 (create-rect (+v src-pos
					  (make-v 0 (if (eq (aval e :facing) :left)
							0
							(tiles 3/2))))
				      *elephant-dims*)
			 :pos (physics-pos e))))



(defun elephant-origin (e)
  (+v (physics-pos e)
      (scale-v *elephant-dims* 1/2)))

(defun elephant-inertia-vel (e)
  (scale-v (stage-vel (aval e :physics)) 1/3))

(defun elephant-damageable-rect (e)
  (create-rect (physics-pos e) *elephant-dims*))


(defun elephant-hit-react (e amt)
  (let ((timers (aval e :timers))
	(physics (aval e :physics))
	(health-amt (aval e :health-amt))
	(dead? (aval e :dead?)))
    (setq physics
	  (aset physics
		:shake
		(make-wave-motion :dir :left :amp 2 :speed 0.1 :rads 0)))
    (setq timers (aset timers :shake (create-expiring-timer (s->ms 1/3) t)))
    (if (< amt (aval e :health-amt))
	(progn
	  (update-damage-number-amt (aval e :damage-numbers) (aval e :id) amt)
	  (push-sound :enemy-hurt)

	  (setq health-amt (- health-amt amt)))
	(let ((origin (origin e)))
	  (push-sound :enemy-explode)
	  (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	  (create-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	  (create-death-cloud-particles amt origin)

	  (setq dead? t)))

    (unless (timer-active? (aval timers :rage))
      (if (timer-active? (aval timers :recover))
	  (when (< (timer-ms-remaining (aval timers :recover)) (s->ms 1/2))
	    (setq timers (aset timers :rage (create-expiring-timer (s->ms 3)))))
	  (setq timers (aset timers :recover (create-expiring-timer (s->ms 3/4) t)))))
    
    (aset e
	  :physics physics
	  :timers timers
	  :dead? dead?
	  :health-amt health-amt)))



(defun elephant-dynamic-collision-rect (e)
  (let ((pos (physics-pos e)))
    (cond
      ((timer-active? (aval (aval e :timers) :recover))
       (setq pos (+v pos (make-v 0 (tiles 1/4)))))
      ((= 1 (cycle-idx (timed-cycle-cycle
			(aval (aval e :timers) :anim-cycle))))
       (setq pos (+v pos (make-v 0 (tiles 1/8))))))
    (create-rect pos *elephant-dims*)))


(defun elephant-damage-collision-amt (e)
  (if (timer-active? (aval (aval e :timers) :rage))
      5
      1))


(defun elephant-damage-collision-rect (e)
  (let* ((dims (make-v (tiles 1) (tiles 3/4)))
	 (y (tiles 3/4))
	 (pos (if (eq (aval e :facing) :left)
		  (make-v 0 y)
		  (make-v (tiles 1) y))))
    (create-rect (+v (physics-pos e) pos) dims)))


(defun elephant-ai (e ticks)
  (let ((timers (aval e :timers))
	(physics (aval e :physics)))
    (unless (timer-active? (aval timers :shake))
      (removef physics :shake :key #'car :test #'eq))
    
    (when (member :recover ticks)
      (when (aval timers :rage)
	(setq timers (aupdate timers #'reset-timer :rage))
	(setq timers (aset timers :anim-cycle (create-timed-cycle 14 #(8 10))))))

    (when (member :rage ticks)
      (setq timers (arem timers :rage))
      (setq timers (aset timers :anim-cycle (create-timed-cycle 12 #(0 2 4)))))

    (when (timer-active? (aval timers :rage))
      (when (and (member :anim-cycle ticks)
		 (zerop (cycle-idx (timed-cycle-cycle (aval timers :anim-cycle)))))
	(push-sound :big-footstep)
	(replace-entity-state (aval e :camera) (rcurry #'timed-camera-shake (s->ms 1/2)))
	(create-death-cloud-particles 3
				      (+v (physics-pos e)
					  (make-v (if (eq (aval e :facing) :left)
						      (tiles 3/2)
						      (tiles 1/2))
						  (tiles 5/4))))))

    (setq physics
	  (aupdate physics
		   (lambda (kin-2d)
		     (let ((x-vel 0))
		       (cond
			 ((timer-active? (aval timers :rage))
			  (let ((*elephant-speed* (* 2 *elephant-speed*)))
			    (if (eq (aval e :facing) :right)
				(setq x-vel *elephant-speed*)
				(setq x-vel (- *elephant-speed*)))))
			 ((timer-active? (aval timers :recover))
			  (setq x-vel 0))
			 (t
			  (if (eq (aval e :facing) :right)
			      (setq x-vel *elephant-speed*)
			      (setq x-vel (- *elephant-speed*)))))
		       (aset kin-2d
			     :vel
			     (make-v x-vel (y (aval kin-2d :vel))))))
		   :stage))

    (aset e
	  :physics physics
	  :timers timers)))

(let ((collision-rects
       (rect->collision-rects
	(centered-rect (scale-v *elephant-dims* 1/2) *elephant-dims*)
	6)))
  (defun elephant-stage-collision (e stage)
    (let* ((data (alist :facing (aval e :facing)
			:pos (aval (aval (aval e :physics) :stage) :pos)))
	   (stage-collision-result
	    (stage-collisions
	     data stage collision-rects
	     (alist
	      :left
	      (collision-lambda (data)
		(if (eq (aval data :facing) :left)
		    (aset data :facing :right)
		    data))
	      :right
	      (collision-lambda (data)
		(if (eq (aval data :facing) :right)
		    (aset data :facing :left)
		    data))))))
      (aset e
	    :physics 
	    (aset (aval e :physics)
		  :stage
		  (aset (aval (aval e :physics) :stage)
			:pos (aval stage-collision-result :pos)))
	    :facing (aval stage-collision-result :facing)))))
