;;;; cave-story.lisp

(in-package #:cave-story)

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
     ,(lambda () (setq *global-game* (reset!))))
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
		      (update-and-render! *global-game*)))))))

(defun make-game
    (&key player camera stage projectile-groups
       damage-numbers active-systems (input (make-input)))
  (alist :player player
	 :camera camera
	 :stage stage
	 :projectile-groups projectile-groups
	 :damage-numbers damage-numbers
	 :active-systems active-systems
	 :input input))

(let (debug-toggle-off?)
  (defun handle-debug-input! (transient-input)
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

(defun main! ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       last-update-time)
	   (setq *global-game* (init!))
	   (setq last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (let ((transient-input (gather-transient-input!)))
		    (handle-debug-input! transient-input)
		    (setq *global-game*
			  (aset *global-game*
				:input (gather-input (aval *global-game* :input)
						     transient-input)))))
		
		(when (>= frame-timer (* *update-period* *frame-time*))
		  (if *global-paused?*
		      (draw-text-line! (zero-v) "PAUSED")
		      (setq *global-game* (update-and-render! *global-game*)))
		  (render! *renderer*
			   *font*
			   *render-list*
			   (camera-pos (estate (aval *global-game* :camera))
				       (stage-dims->camera-bounds
					(stage-dims (aval *global-game* :stage)))))
		  (setq frame-timer (- frame-timer
				       (* *update-period* *frame-time*))))

		(let ((dt (- (sdl:get-ticks) last-update-time)))
		  ;; NOTE: if we are paused beyond our control, Don't play catchup.
		  (setq frame-timer
			(+ frame-timer
			   (min dt (* 2 *frame-time*)))))
		(setq last-update-time (sdl:get-ticks))
		(music-update!)
		(sdl:delay 1)))
      (cleanup!))))

(defun handle-input! (game)
  "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."

  (let ((input (aval game :input))
	(input-systems
	 (aval (estate (aval game :active-systems)) :input)))
    (update-input-subsystem! input-systems input)
    (ecase (first input-systems)
      (:game
       (when (or (joy-pressed? input :b) (key-pressed? input :x))
	 ;; Fire Gun
	 (update-world! (aval game :player) #'player-fire-gun)))

      (:dialog
       (cond
	 ((or (joy-pressed? input :b) (key-pressed? input :x))
	  (dialog-ok-pressed!))
	 ((or (joy-held? input :a) (key-pressed? input :z)
	      (joy-held? input :b) (key-pressed? input :x))
	  (dialog-button-held!))
	 (t
	  (dialog-buttons-released!)))))))

(defun dialog-ok-pressed! ()
  )
(defun dialog-button-held! ()
  (fast-text-speed!))
(defun dialog-buttons-released! ()
  (slow-text-speed!))

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

(defun make-pickup (&key type amt)
  (alist :type type :amt amt))

(defun dorito-fns-alist ()
  (alist :ai-fn #'dorito-ai
	 :draw-fn #'dorito-drawing
	 :stage-collision-fn #'dorito-stage-collision
	 :pickup-rect-fn #'dorito-pickup-rect
	 :pickup-kill-fn #'dorito-pickup-kill))

(defparameter *dorito-subsystems*
  '(:timers :physics :stage-collision :drawable :pickup))

(defun make-dorito (pos vel size)
  (amerge
   (dorito-fns-alist)
   (alist :subsystems *dorito-subsystems*)
   (dorito-pickup-data size)
   (alist :timers
	  '(:life-timer :anim-cycle)
	  :life-timer
	  (make-expiring-timer (s->ms 8) t)
	  :anim-cycle
	  (make-fps-cycle 14 (iota 6))
	  :physics '(:stage-physics)
	  :stage-physics
	  (make-kin-2d :pos (-v pos
				(rect-pos (dorito-collision-rect size)))
		       :vel vel
		       :accelerator-x
		       (friction-accelerator *dorito-friction-acc*)
		       :accelerator-y
		       (const-accelerator *gravity-acc*)
		       :clamper-vy
		       (clamper+- *terminal-speed*))
	  :size size)))

(defun dorito-ai (d)
  (aset d :dead? (not (timer-active? (aval d :life-timer)))))

(defun dorito-pos (d)
  (physics-pos d))

(defun flash-time? (tr)
  (and (timer-active? tr) (zerop (chunk-timer-period tr 50))))

(defun death-flash? (tr)
  (and (< (aval tr :ms-remaining) (s->ms 1))
       (flash-time? tr)))

(defun dorito-drawing (d)
  (unless (death-flash? (aval d :life-timer))
    (make-sprite-drawing
     :layer :pickup
     :sheet-key :npc-sym

     :src-rect
     (create-rect
      (+v (anim-cycle-offset d)
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

(defparameter *dorito-stage-collisions*
  (alist
   :bottom
   (collision-lambda (data)
     (aupdate data
	      :sound-effects
	      (pushfn :dorito-bounce)
	      :stage-physics
	      (asetfn :vel
		      (set-y-v (stage-vel data)
			       (- *dorito-bounce-speed*)))))
   :right
   (collision-lambda (data)
     (if (plusp (x (stage-vel data)))
	 (aupdate data
		  :stage-physics
		  (asetfn :vel (reverse-x-v (stage-vel data))))
	 data))
   :left
   (collision-lambda (data)
     (if (minusp (x (stage-vel data)))
	 (aupdate data
		  :stage-physics
		  (asetfn :vel (reverse-x-v (stage-vel data))))
	 data))
   :top
   (collision-lambda (data)
     (aupdate data
	      :stage-physics
	      (asetfn :vel (max-y-v (stage-vel data) 0))))))

(defun dorito-stage-collision (d stage)
  (let ((collision-rects (rect->collision-rects
			  (dorito-collision-rect (aval d :size)))))
    (stage-collisions d stage collision-rects
		      *dorito-stage-collisions*)))

(defun dorito-pickup-rect (d)
  (rect-offset (dorito-collision-rect (aval d :size)) (physics-pos d)))

(setfn dorito-pickup-kill
       (comp 
	(aupdatefn :sound-effects (pushfn :pickup))
	(asetfn :dead? t)))

(defun dorito-pickup-data (size)
  (make-pickup :type :dorito
	       :amt (ecase size
		      (:small 1)
		      (:medium 10)
		      (:large 20))))

(defun dorito-collision-rect (size)
  (centered-rect (tile-dims/2)
		 (ecase size
		   (:small (both-v (tiles 2/5)))
		   (:medium (both-v (tiles 3/4)))
		   (:large (both-v (tiles 1))))))

(defun single-loop-sprite-fns-alist ()
  (alist :ai-fn #'single-loop-sprite-ai))

(defparameter *single-loop-sprite-subsystems*
  '(:timers))

(defun make-single-loop-sprite (fps seq sheet-key tile-y layer)
  (amerge
   (single-loop-sprite-fns-alist)
   (alist :subsystems *single-loop-sprite-subsystems*)
   (alist :anim-cycle (make-fps-cycle fps seq)
	  :timers '(:anim-cycle)
	  :sheet-key sheet-key
	  :id (gen-entity-id)
	  :layer layer
	  :tile-y tile-y)))

(defun single-loop-sprite-drawing (s pos)
  (unless (aval s :dead?)
    (make-sprite-drawing :layer (aval s :layer)
			 :sheet-key (aval s :sheet-key)
			 :src-rect
			 (tile-rect (tile-v (cycle-current
					     (aval s :anim-cycle))
					    (aval s :tile-y)))
			 :pos pos)))

(defun anim-cycle-idx (obj)
  (aval (aval obj :anim-cycle) :idx))

(defun single-loop-sprite-ai (p)
  (aset p
	:dead? (and (ticked? p :anim-cycle)
		    (zerop (anim-cycle-idx p)))))

(defun particle-fns-alist ()
  (alist :draw-fn #'particle-drawing
	 :ai-fn
	 (lambda (p)
	   (aset p :dead? (aval (estate (aval p :single-loop-sprite)) :dead?)))))

;; NOTE: This (ai/timers) is a kludge to make it so particle can set its
;; dead? flag based on single-loop-sprite.
(defparameter *particle-subsystems* '(:drawable :timers :ai))
(defun make-particle (&key seq fps sheet-key tile-y pos)
  (amerge
   (particle-fns-alist)
   (alist :subsystems *particle-subsystems*)
   (let ((sprite (make-single-loop-sprite fps seq sheet-key tile-y :particle)))
     (alist :single-loop-sprite (aval sprite :id)
	    :new-entities (list sprite)
	    :pos pos))))

(defun particle-drawing (p)
  (let ((sp (estate (aval p :single-loop-sprite))))
    (if (aval sp :dead?)
	nil
	(make-sprite-drawing :layer :particle
			     :sheet-key (aval sp :sheet-key)
			     :src-rect
			     (tile-rect (tile-v (cycle-current
						 (aval sp :anim-cycle))
						(aval sp :tile-y)))
			     :pos (aval p :pos)))))

(defun make-projectile-star-particle (center-pos)
  (make-particle :seq (alexandria:iota 4)
		 :fps 14
		 :sheet-key :caret
		 :tile-y 3
		 :pos (sub-v center-pos (tile-dims/2))))

(defun make-projectile-wall-particle (center-pos)
  (make-particle :seq (mapcar (curry #'+ 11) (alexandria:iota 4))
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

(defparameter *floating-number-subsystems*
  '(:timers :drawable :physics))
(defun make-floating-number (entity amt)
  (amerge
   (floating-number-fns-alist)
   (alist :subsystems *floating-number-subsystems*)
   (alist :entity entity
	  :amt amt
	  :id (gen-entity-id)
	  :timers '(:life-timer)
	  :life-timer (make-expiring-timer (s->ms 2) t)
	  :physics '(:offset)
	  :offset (make-offset-motion (zero-v)
				      :up
				      (/ (tiles 1/30) *frame-time*)))))

(defun floating-number-drawing (fn)
  (number-drawing (+v (origin (estate (aval fn :entity)))
		      (physics-pos fn))
		  (aval fn :amt)
		  :layer :floating-text))

(defun floating-number-ai (fn)
  (let ((dead? (not (timer-active? (aval fn :life-timer)))))
    (cond ((< (y (motion-pos (aval fn :offset)))
	      (- (tiles 1)))
	   (aset fn
		 :dead? dead?
		 :offset
		 (make-offset-motion (zero-v :y (- (tiles 1))) :up 0)))
	  (t (aset fn :dead? dead?)))))

(defun floating-number-add-amt (fn amount)
  (aupdate fn
	   :life-timer #'reset-timer
	   :amt #_(+ _ amount)))

(defun remove-all-dead! (game)
  (estate-set! (aval game :projectile-groups)
	       (projectile-groups-remove-dead
		(estate (aval game :projectile-groups))))
  (estate-set! (aval game :damage-numbers)
	       (damage-numbers-remove-dead
		(estate (aval game :damage-numbers))))
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
(defun slow-text-speed! ()
  (setq *text-speed* 100))
(defun fast-text-speed! ()
  (setq *text-speed* 25))
(defparameter *cursor-blink-time* 100)

#+nil
(defparameter *text-display-subsystems*
  '(:timers :drawable))

#+nil
(defun text-display-fns-alist ()
  (alist :ai-fn #'text-display-ai
	 :draw-fn #'text-display-draw))

#+nil
(defun make-text-display (pos text)
  (amerge
   (text-display-fns-alist)
   (alist :subsystems *text-display-subsystems*)
   (alist :pos pos
	  :text text
	  :num-chars 0
	  :timers
	  (alist :text (make-expiring-timer *text-speed* t))
	  :wait-for-input? t
	  :blink-time 0)))

#+nil
(defun text-display-ai (td)
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
      :timers (aset (text-display-timers td) :text (make-expiring-timer *text-speed* t))
      :pos (text-display-pos td)
      :text (text-display-text td)
      :num-chars (+ 1 (text-display-num-chars td))
      :wait-for-input? (text-display-wait-for-input? td)
      :blink-time (+ (text-display-blink-time td) *frame-time*)
      :dead? (text-display-dead? td)))
    (t td)))

#+nil
(defun text-display-drawing (td)
  (list* (make-text-line-drawing
	  :pos (text-display-pos td)
	  :text (subseq (text-display-text td)
			0
			(text-display-num-chars td))
	  :layer :text)
	 (draw-textbox 5 21 30 8)))

(defun hud-fns-alist ()
  (alist :draw-fn #'hud-drawing))

(defparameter *hud-subsystems* '(:timers :drawable))
(defun make-hud  (player gun-exps id)
  (amerge
   (hud-fns-alist)
   (alist :subsystems *hud-subsystems*)
   (alist :player player
	  :gun-exps gun-exps
	  :id id
	  :timers '(:exp-change-timer :health-change-timer)
	  :exp-change-timer (make-expiring-timer (s->ms 1))
	  :health-change-timer (make-expiring-timer (s->ms 1/2)))))

(defun hud-drawing (hud)
  (let ((bar-tile/2-w 5)
	(bar-tile/2-x 5)
	(drawings))
    (push
     (make-sprite-drawing :layer
			  :hud-bg :sheet-key :text-box
			  :src-rect
			  (create-rect-cmpts 0 (tiles/2 5)
					     (tiles/2 8) (tiles/2 1))
			  :pos (tile-v 1 2))
     drawings)

    (let ((health (aval (player-state (aval hud :player)) :health-amt)))
      (when (timer-active? (aval hud :health-change-timer))
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

      (when (flash-time? (aval hud :exp-change-timer))
	(push
	 (make-sprite-drawing :layer :hud-fg
			      :sheet-key :text-box
			      :src-rect
			      (create-rect (tiles/2-v 5 10)
					   (tiles/2-v bar-tile/2-w 1))
			      :pos exp-pos)
	 drawings))

      (multiple-value-bind (exp gun-name) (current-gun-exp (aval hud :player)
							   (aval hud :gun-exps))
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

(setfn hud-exp-changed
       (aupdatefn :exp-change-timer #'reset-timer))

(setfn hud-health-changed
       (comp
	(aupdatefn :health-change-timer #'reset-timer)
	(lambda (hud)
	  (aset hud
		:last-health-amt
		(aval (player-state (aval hud :player)) :health-amt)))))

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

(defun update-and-render! (game)
  "The Main Loop, called once per *FRAME-TIME*."
  (when (eq *input-playback* :playback)
    (setq game
	  (aset game :input (next-playback-input))))
  (handle-input! game)

  (setq *render-list* nil)

  (case *input-playback*
    (:recording
     (draw-text-line! (zero-v) "RECORD"))
    (:playback
     (draw-text-line! (zero-v) "PLAYBACK")))

  (let ((active-update-systems (aval (estate (aval game :active-systems)) :update))
	(stage (aval game :stage))
	(player (aval game :player)))
    (update-timers-subsystem! active-update-systems)
    (update-physics-subsystem! active-update-systems)
    (update-bullet-subsystem! active-update-systems)
    (update-stage-collision-subsystem! active-update-systems stage)
    (update-pickup-subsystem! active-update-systems player)

    (update-damage-collision-subsystem! active-update-systems player)
    (update-dynamic-collision-subsystem! active-update-systems player))

  (let ((active-draw-systems (aval (estate (aval game :active-systems)) :draw)))
    (update-drawable-subsystem! active-draw-systems))

  (remove-all-dead! game)

  ;; Debug Drawings Below.

  ;; (draw-point! (player-nozzle-pos player) *red*)
  (let ((focus (camera-focus (estate (aval game :camera))))
	(camera-bounds (stage-dims->camera-bounds (stage-dims (aval game :stage)))))
    (draw-point! focus *cyan*)
    (draw-point! (clamp-pos focus camera-bounds) *red*)
    (draw-rect! camera-bounds *cyan*))
  (draw-point! (camera-target-from-player (player-state (aval game :player))) *white*)
  ;; End Debug Drawings.

  (play-sounds! *sfx-play-list*)
  (setq *sfx-play-list* nil)

  (when (eq *input-playback* :recording)
    (record-frame-input (aval game :input)))

  (aset game :input (reset-transient-input (aval game :input))))

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

(defun make-active-systems
    (&key (update (list :game)) (draw (list :game)) (input (list :game)) (id (gen-entity-id)))
  (alist :update update
	 :draw draw
	 :input input
	 :id id))

(defun active-systems-switch-to-dialog (d)
  (aset d
	:update (list :dialog)
	:draw (list :game :dialog)
	:input (list :dialog)))


(defun damage-numbers-remove-dead (d)
  (aset d
	:pairs
	(remove-if (lambda (pair) (aval (estate (cdr pair)) :dead?)) (aval d :pairs))))

(defun make-damage-numbers ()
  (alist :id (gen-entity-id)))

(defun gun-exp-for (gun-exps gun-name)
  (aval (aval gun-exps :guns) gun-name))

(defun incr-gun-exp (gun-exps gun-name amt)
  (let* ((guns (aval gun-exps :guns))
	 (gun-exp (aval guns gun-name)))
    (aset gun-exps
	  :guns (aset guns
		      gun-name
		      (min (max (+ gun-exp amt) 0)
			   (exp-for-gun-level gun-name :max))))))

(defun make-gun-exps ()
  (alist :id (gen-entity-id)
	 :guns (loop for g across *gun-names* collecting (cons g 0))))

(defun projectile-groups-remove-dead (g)
  (aset g
	:groups
	(remove-if #'null
		   (loop for (name . group) in (aval g :groups)
		      collecting
			(let ((new-g (remove-if (lambda (x)
						  (dead? (estate x)))
						group)))
			  (when new-g
			    (list* name new-g)))))))

(defun projectile-groups-count (g gun-name)
  (count gun-name (aval g :groups) :key #'car))

(defun projectile-groups-add (g pg)
  (aupdate g :groups (pushfn pg)))

(defun make-projectile-groups ()
  (alist :id (gen-entity-id)))

(defun current-gun-exp (player gun-exps)
  (let* ((gun-name (player-current-gun-name (player-state player)))
	 (exp (gun-exp-for (estate gun-exps) gun-name)))
    (values
     exp
     gun-name)))

(defun save-current-state ()
  (list (current-entity-states) *global-game*))

(defun restore-state (state)
  (dolist (s *registry-syms*)
    (set s nil))
  (restore-entity-states! (first state))
  (setq *global-game* (second state)))

(defun create-game! ()
  (let ((damage-numbers (create-entity! (make-damage-numbers)))
	(projectile-groups (create-entity! (make-projectile-groups)))
	(stage (make-stage (basic-stage)))
	(hud (gen-entity-id))
	(gun-exps (create-entity! (make-gun-exps)))
	(active-systems (create-entity! (make-active-systems))))
    (create-entity! stage)
    (let* ((player (create-entity!
		    (make-player hud
				 projectile-groups
				 damage-numbers
				 gun-exps
				 active-systems)))
	   (camera (create-entity!
		    (make-camera (v/2 *window-dims*) (zero-v) player))))
      (create-entity! (make-hud player gun-exps hud))

      (create-entity! (make-critter (make-v (+ (tiles 14) (tiles 1/4))
					    (tiles 6))
				    player
				    damage-numbers))
      (create-entity!
       (make-elephant (make-v (tiles 7) (tiles 6)) player camera damage-numbers))
      (dolist (x '(1 3 6 7))
	(create-entity! (make-bat x 7 player)))
      (create-entity! (make-dorito (make-v (+ (tiles 14) (tiles 1/4))
					   (tiles 6))
				   (make-v 0 0)
				   :medium))
      (make-game :player player
		 :camera camera
		 :stage stage
		 :projectile-groups projectile-groups
		 :active-systems active-systems
		 :damage-numbers damage-numbers))))

(defun reset! ()
  (switch-to-new-song! :lastcave)
  (set-music-volume! 20)

  (dolist (s *registry-syms*)
    (set s nil))
  (init-entity-registry!)

  (setq *global-paused?* nil)

  (create-game!))

(defun init! ()
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

  (put-all-resources!)
  (setq *current-song* nil)

  (multiple-value-setq
      (*window* *renderer*)
    (sdl:default-window-and-renderer
	"Cave Story"
	(x *window-dims*) (y *window-dims*)
	0))
  (reset!))

(defun cleanup! ()
  "Called at application closing to cleanup all subsystems."
  (cleanup-input!)
  (cleanup-all-resources!)
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
	 (comp point-rect origin)
	 :damage-collision-amt-fn (constantly 1)
	 :damageable-rect-fn #'physics-tile-rect))

(defparameter *bat-subsystems*
  '(:timers :damage-collision :damageable :drawable :physics))

(defun make-bat (tile-x tile-y player)
  (amerge
   (bat-fns-alist)
   (alist :subsystems *bat-subsystems*)
   (alist
    :physics '(:wave)
    :wave (make-wave-motion
	   :origin (tile-v tile-x
			   tile-y)
	   :dir :up
	   :amp (tiles 2)
	   :speed (/ 0.0325
		     *frame-time*))
    :timers '(:anim-cycle)
    :anim-cycle (make-fps-cycle 14 #(0 2 1 2))
    :health-amt 1
    :player player)))

(defun bat-drawing (b)
  (make-sprite-drawing :layer :enemy
		       :sheet-key :npc-cemet
		       :src-rect
		       (tile-rect (+v (tile-v 2 2)
				      (anim-cycle-offset b)
				      (facing-offset b)))
		       :pos (physics-pos b)))

(defun facing-offset (obj)
  (tile-v 0 (if (eq (aval obj :facing) :left) 0 1)))

(defun anim-cycle-offset (c)
  (tile-v (cycle-current (aval c :anim-cycle)) 0))

(defun point-rect (pos)
  (create-rect pos (both-v 1)))

(defun polar-vec->v (angle mag)
  (make-v (* mag (cos angle))
	  (* mag (sin angle))))

(defun death-cloud-particle-fns-alist ()
  (alist :draw-fn #'death-cloud-particle-drawing
	 :stage-collision-fn #'death-cloud-particle-stage-collision
	 :ai-fn (lambda (p)
		  (aset p
			:dead?
			(aval (estate (aval p :single-loop-sprite))
			      :dead?)))))

(defparameter *death-cloud-particle-subsystems*
  '(:drawable :physics :stage-collision :timers :ai))

(defun make-death-cloud-particle (pos)
  (amerge
   (death-cloud-particle-fns-alist)
   (alist :subsystems *death-cloud-particle-subsystems*)
   (let ((sprite (make-single-loop-sprite
		  15 (mapcar #'1+ (alexandria.0.dev:iota 7))
		  :npc-sym 0 :particle)))
     (alist :single-loop-sprite (aval sprite :id)
	    :new-entities (list sprite)
	    :physics '(:stage-physics)
	    :stage-physics
	    (make-kin-2d
	     :pos (-v pos (tile-dims/2))
	     :vel (polar-vec->v (rand-angle)
				(rand-val-between 0.1 0.3))
	     :clamper-vx
	     (clamper+- *terminal-speed*)
	     :clamper-vy
	     (clamper+- *terminal-speed*))))))

(defun death-cloud-particle-drawing (d)
  (single-loop-sprite-drawing (estate (aval d :single-loop-sprite))
			      (physics-pos d)))

(let ((collision-rects (rect->collision-rects
			(centered-rect (tile-dims/2) (both-v (tiles 2/5))))))
  (defun death-cloud-particle-stage-collision (d stage)
    (stage-collisions
     d stage collision-rects
     (let ((stop-x
	    (collision-lambda (data)
	      (aset data
		    :vel
		    (set-x-v (stage-vel data) 0))))
	   (stop-y
	    (collision-lambda (data)
	      (aset data
		    :vel
		    (set-y-v (stage-vel data) 0)))))
       (alist :bottom stop-y :left stop-x
	      :right stop-x :top stop-y)))))

(defun make-num-death-cloud-particles (num pos)
  (loop for i from 1 to num
     collect (make-death-cloud-particle pos)))

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

(defun make-critter (pos player damage-numbers)
  (let ((id (gen-entity-id)))
    (amerge
     (critter-fns-alist)
     (alist :subsystems *critter-subsystems*)
     (alist
      :stage-physics (gravity-kin-2d :pos pos)
      :physics '(:stage-physics)
      :player player
      :health-amt 2
      :damage-numbers damage-numbers
      :id id))))

(defparameter *critter-subsystems*
  '(:timers :drawable :physics :damageable
    :damage-collision :dynamic-collision :stage-collision))

(defun origin-dist (a b)
  (dist (origin a) (origin b)))

(defun shake-ai (obj)
  (if (not (timer-active? (aval obj :shake-timer)))
      (aupdate obj
	       :physics (removefn :shake)
	       :timers (removefn :shake-timer))
      obj))

(defun face-player-ai (obj)
  (aset obj :facing (face-player (physics-pos obj) (aval obj :player))))

(defun critter-jump-ai (c)
  (let ((facing (aval c :facing)))

    (if (and (not (timer-active? (aval c :sleep-timer)))
	     (< (origin-dist c (estate (aval c :player))) (tiles 4))
	     (aval c :ground-tile))
	(aupdate c
		 :stage-physics
		 (asetfn :vel
			 (make-v (* 0.04 (if (eq facing :left) -1 1))
				 (- 0.35))))
	c)))

(defun critter-drawing (c)
  (let* ((sleep-timer (aval c :sleep-timer))
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
						  (facing-offset c)))
			 :pos (physics-pos c))))



(defun critter-damage-collision-rect (c)
  (rect-offset *critter-dynamic-collision-rect* (physics-pos c)))

(defun dynamic-collision-enemy-react
    (pos origin id dynamic-collision-rect side player-collision-rect player-state)
  (let* ((kin-2d (aval player-state :stage-physics))
	 (ground-tile (aval player-state :ground-tile))
	 (ground-inertia-entity (aval player-state :ground-inertia-entity)))
    (setq player-state
	  (aset player-state
		:stage-physics
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
			       (flush-rect-pos
				player-rect
				(y (rect-pos dynamic-collision-rect))
				:up)
			       (rect-pos player-collision-rect))))
		       (t kin-2d)))
		    ((:left :right)
		     (let ((disp (- (x (aval kin-2d :pos)) (x pos))))
		       (cond ((> (abs disp) (tiles 1/4))
			      (aset
			       kin-2d
			       :vel
			       (make-v (* (/ *terminal-speed* 70) disp) (y (aval kin-2d :vel)))))
			     (t kin-2d))))
		    (t kin-2d)))))
    (aset player-state
	  :ground-tile ground-tile
	  :ground-inertia-entity ground-inertia-entity)))

(defparameter *critter-stage-collisions*
  (alist
   :bottom
   (collision-lambda (data)
     (aset data
	   :timers (if (aval data :last-ground-tile)
		       (aval data :timers)
		       (adjoin :sleep-timer (aval data :timers)))
	   :sleep-timer (if (aval data :last-ground-tile)
			    (aval data :sleep-timer)
			    (make-expiring-timer (s->ms 1/3) t))
	   :ground-tile (aval data :tile-type)
	   :stage-physics
	   (aset (aval data :stage-physics) :vel (zero-v))))
   :top
   (collision-lambda (data)
     (aset data :stage-physics
	   (aset (aval data :stage-physics)
		 :vel (max-y-v (stage-vel data) 0))))))

(let ((collision-rects (rect->collision-rects
			(centered-rect (tile-dims/2) (both-v (tiles 3/4))) 6)))
  (defun critter-stage-collision (c stage)
    (let ((last-tile (aval c :ground-tile)))
      (stage-collisions
       (aset c
	     :last-ground-tile last-tile
	     :ground-tile nil)
       stage collision-rects
       *critter-stage-collisions*))))

(defun physics-tile-origin (c)
  (+v (physics-pos c) (tile-dims/2)))
(defun physics-tile-rect (c)
  (tile-rect (physics-pos c)))

(defun stage-vel (e)
  (aval (aval e :stage-physics) :vel))

(defun critter-inertia-vel (c)
  (stage-vel c))

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

(defparameter *elephant-subsystems*
  '(:timers :drawable :physics :stage-collision
    :damageable :damage-collision :dynamic-collision))

(defun make-elephant  (pos player camera damage-numbers)
  (amerge
   (elephant-fns-alist)
   (alist :subsystems *elephant-subsystems*)
   (alist :stage-physics
	  (gravity-kin-2d :pos pos
			  :vel (make-v (- *elephant-speed*) 0))
	  :physics '(:stage-physics)
	  :timers '(:anim-cycle)
	  :anim-cycle (make-fps-cycle 12 #(0 2 4))
	  :health-amt 8
	  :facing :left
	  :damage-numbers damage-numbers
	  :camera camera
	  :player player)))

(defparameter *elephant-dims* (make-v (tiles 2) (tiles 3/2)))
(defun elephant-drawing (e)
  (let ((src-pos
	 (cond
	   ((timer-active? (aval e :recover-timer))
	    (make-v (tiles (* 2 3)) 0))
	   (t (anim-cycle-offset e)))))
    (make-sprite-drawing
     :layer :enemy
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
  (scale-v (stage-vel e) 1/3))

(defun elephant-damageable-rect (e)
  (create-rect (physics-pos e) *elephant-dims*))

(setfn shake-hit-react
       (comp
	(aupdatefn
	 :timers (adjoinfn :shake-timer)
	 :physics (adjoinfn :shake))
	(asetfn
	 :shake-timer
	 (make-expiring-timer (s->ms 1/3) t)
	 :shake
	 (make-wave-motion :dir :left :amp 2 :speed 0.1 :rads 0))))

(defun elephant-rage-hit-react (e)
  (if (timer-active? (aval e :rage-timer))
      e
      (let* ((recover? (not (timer-active? (aval e :recover-timer))))
	     (rage? (and (not recover?)
			 (< (aval (aval e :recover-timer) :ms-remaining)
			    (s->ms 1/2)))))
	(aupdate e
		 :recover-timer
		 (when recover?
		   (constantly (make-expiring-timer (s->ms 3/4) t)))
		 :rage-timer
		 (when rage?
		   (constantly (make-expiring-timer (s->ms 3))))
		 :timers
		 (cond (recover? (adjoinfn :recover-timer))
		       (rage? (adjoinfn :rage-timer)))))))

(defun damage-number-update-amtfn (amt)
  (lambda (obj)
    (let* ((d (estate (aval obj :damage-numbers)))
	   (e (aval obj :id))
	   (dns (aval d :pairs))
	   (existing-dn-pair (assoc e dns)))
      (if existing-dn-pair
	  (aupdate obj
		   :new-states
		   (pushfn (floating-number-add-amt
			    (estate (cdr existing-dn-pair)) (- amt))))
	  (let ((fn (make-floating-number e (- amt))))
	    (aupdate obj
		     :new-states
		     (pushfn (aupdate d :pairs
				      (pushfn (cons e (aval fn :id)))))
		     :new-entities (pushfn fn)))))))

(defun damage-reaction (num-particles obj)
  (let ((amt (aval obj :damage-amt)))
    (funcall
     (if (< amt (aval obj :health-amt))
	 (comp
	  (damage-number-update-amtfn amt)
	  (aupdatefn
	   :health-amt #_(- _ amt)
	   :sound-effects (pushfn :enemy-hurt)))
	 (let ((origin (origin obj)))
	   (comp
	    (asetfn
	     :dead? t)
	    (aupdatefn
	     :new-entities
	     (appendfn
	      (list
	       (make-dorito origin (polar-vec->v (rand-angle) 0.07) :small)
	       (make-dorito origin (polar-vec->v (rand-angle) 0.07) :small))
	      (make-num-death-cloud-particles num-particles (origin obj)))
	     :sound-effects (pushfn :enemy-explode)))))
     obj)))

(defun elephant-dynamic-collision-rect (e)
  (let ((pos (physics-pos e)))
    (cond
      ((timer-active? (aval e :recover-timer))
       (setq pos (+v pos (make-v 0 (tiles 1/4)))))
      ((= 1 (anim-cycle-idx e))
       (setq pos (+v pos (make-v 0 (tiles 1/8))))))
    (create-rect pos *elephant-dims*)))

(defun elephant-damage-collision-amt (e)
  (if (timer-active? (aval e :rage-timer))
      5
      1))

(defun elephant-damage-collision-rect (e)
  (let* ((dims (make-v (tiles 1) (tiles 3/4)))
	 (y (tiles 3/4))
	 (pos (if (eq (aval e :facing) :left)
		  (make-v 0 y)
		  (make-v (tiles 1) y))))
    (create-rect (+v (physics-pos e) pos) dims)))

(defun rage-timer (e)
  (aval e :rage-timer))
(defun recover-timer (e)
  (aval e :recover-timer))

(defun elephant-stage-physics-ai (e)
  (let ((x-vel
	 (cond
	   ((timer-active? (rage-timer e))
	    (if (eq (aval e :facing) :right)
		(* 2 *elephant-speed*)
		(- (* 2 *elephant-speed*))))
	   ((timer-active? (recover-timer e))
	    0)
	   (t
	    (if (eq (aval e :facing) :right)
		*elephant-speed*
		(- *elephant-speed*))))))
    (aupdate e
	     :stage-physics
	     (asetfn :vel (make-v x-vel (y (stage-vel e)))))))

(setfn elephant-rage-end
       (comp
	(aupdatefn
	 :timers (removefn :rage-timer))
	(asetfn
	 :anim-cycle (make-fps-cycle 12 #(0 2 4))
	 :rage-timer nil)))

(setfn elephant-rage-begin
       (compose
	(aupdatefn
	 :timers (adjoinfn :rage-timer)
	 :rage-timer #'reset-timer)
	(asetfn
	 :anim-cycle (make-fps-cycle 14 #(8 10)))))

(defun elephant-rage-ai (e)
  (cond ((ticked? e :rage-timer)
	 (elephant-rage-end e))
	((and (rage-timer e) (ticked? e :recover-timer))
	 (elephant-rage-begin e))
	(t e)))

(defun elephant-rage-effects (e)
  (if (and (timer-active? (rage-timer e))
	   (ticked? e :anim-cycle)
	   (zerop (anim-cycle-idx e)))
      (aupdate e
	       :sound-effects (pushfn :big-footstep)
	       :new-states
	       (pushfn (timed-camera-shake (estate (aval e :camera))
					   (s->ms 1/2)))
	       :new-entities
	       (appendfn
		(make-num-death-cloud-particles
		 3 (+v (physics-pos e)
		       (make-v (if (eq (aval e :facing) :left)
				   (tiles 3/2)
				   (tiles 1/2))
			       (tiles 5/4))))))
      e))

(let ((collision-rects
       (rect->collision-rects
	(centered-rect (scale-v *elephant-dims* 1/2) *elephant-dims*)
	6)))
  (defun elephant-stage-collision (e stage)
    (stage-collisions
     e stage collision-rects
     (alist
      :left
      (lambda (data)
	(if (eq (aval data :facing) :left)
	    (aset data :facing :right)
	    data))
      :right
      (lambda (data)
	(if (eq (aval data :facing) :right)
	    (aset data :facing :left)
	    data))))))

(setfn bat-hit-react
       #_(damage-reaction 3 _))
(setfn bat-ai #'face-player-ai)
(setfn critter-ai
       (comp face-player-ai
	     critter-jump-ai
	     shake-ai))
(setfn critter-hit-react
       (comp #_(damage-reaction 6 _)
	     shake-hit-react))
(setfn elephant-hit-react 
       (comp #_(damage-reaction 6 _)
	     elephant-rage-hit-react
	     shake-hit-react))
(setfn elephant-ai
       (comp elephant-stage-physics-ai
	     elephant-rage-effects
	     elephant-rage-ai
	     shake-ai))

(defun read-uint16 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    u2))

(defun read-uint32 (in)
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    (setf (ldb (byte 8 16) u2) (read-byte in))
    (setf (ldb (byte 8 24) u2) (read-byte in))
    u2))

(defun read-pxm-file (path)
  "Parses a pxm map file."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (dotimes (i 4) (read-byte stream))
    (let* ((xsize (read-uint16 stream))
	   (ysize (read-uint16 stream))
	   (tile-offset-idxs (loop for i from 1 to (* xsize ysize)
	   			collecting (read-byte stream))))
      (when (read-byte stream nil nil)
	(warn "Finished reading but there was more data."))
      (alist
       :width xsize
       :height ysize
       :tile-offset-idxs tile-offset-idxs))))

(defun read-pxa-file (path)
  (read-file-into-byte-vector path))

;; Tile Attribute Interpretations for pxa
#||
// these flag constants come from the stage data somewhere I believe
// (don't remember for sure) so they should stay constant.
TA_SOLID_PLAYER	0x00001	// solid to player
TA_SOLID_NPC	0x00002	// solid to npc's, enemies and enemy shots
TA_SOLID_SHOT	0x00004	// solid to player's shots
TA_SOLID	(TA_SOLID_PLAYER | TA_SOLID_SHOT | TA_SOLID_NPC)

TA_HURTS_PLAYER	0x00010	// this tile hurts the player -10hp
TA_FOREGROUND	0x00020	// tile is drawn in front of sprites
TA_DESTROYABLE	0x00040	// tile is destroyable if player shoots it
TA_WATER	0x00080	// tile is water/underwater
TA_CURRENT	0x00100	// blows player (tilecode checked to see which direction)
TA_SLOPE	0x00200	// is a slope (the tilecode is checked to see what kind)
||#

;; Background Scrolling Types
#||
BK_FIXED			0 // backdrop does not scroll
BK_PARALLAX			1 // bk is parallax scroll
BK_FOLLOWFG			2 // scrolls, but is 1:1 with foreground
BK_HIDE				3 // draw #000021 blue instead of a graphic
BK_HIDE2			4 // identical to BK_HIDE
BK_FASTLEFT			5 // fast scroll left, items falling left (ironhead battle)
BK_FASTLEFT_LAYERS		6 // fast scroll left w/ layers, items falling left (Outer Wall)
BK_FASTLEFT_LAYERS_NOFALLLEFT	7 // fast left w/ layers, but items don't fall left (Balcony)
BK_HIDE3			8 // identical to BK_HIDE
||#
