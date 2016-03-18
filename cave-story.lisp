;;;; cave-story.lisp

(in-package #:cave-story)

(defvar* *stage-viewer-camera-pos* (scale-v *window-dims* 1/2))

(defvar* *stage-viewer* nil)

;; Debug params.
(defvar* *update-period* 1
  "Number of frames per update. 1 for real-time.")

(defvar* *debug-input-keybindings*
    `((((:key :escape)) .
       quit)
      (((:key :p)
	(:joy :start)) .
       toggle-paused!)
      (((:key :r)
	(:joy :select)) .
       reset!)
      (((:joy :r)) .
       ,(lambda ()
		(case *input-playback*
		  (:recording (begin-input-playback))
		  (:playback (setq *input-playback* nil))
		  (t (begin-input-recording)))))
      (((:key :n)) .
       ,(lambda ()
		(when (aval *env* :paused?)
		  (update-env! (update *env*)))))))

(defun toggle-paused! ()
  (update-env! (aupdate *env* :paused? #'not)))

(defun make-game
    (&key player camera stage projectile-groups hud gun-exps
       damage-numbers)
  (alist :player player
	 :camera camera
	 :stage stage
	 :projectile-groups projectile-groups
	 :gun-exps gun-exps
	 :hud hud
	 :damage-numbers damage-numbers))

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

      (setq *stage-viewer-camera-pos*
	    (+ *stage-viewer-camera-pos*
	       (cond ((find :up pressed-keys)
		      (make-v 0 (- (* *tile-size* 3))))
		     ((find :down pressed-keys)
		      (make-v 0 (* *tile-size* 3)))
		     ((find :left pressed-keys)
		      (make-v (- (* *tile-size* 3)) 0))
		     ((find :right pressed-keys)
		      (make-v (* *tile-size* 3) 0))
		     (t (zero-v)))))

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

(defun current-camera-pos ()
  (if *stage-viewer*
      *stage-viewer-camera-pos*
      (camera-pos (estate (entity-id :camera))
		  (stage-dims->camera-bounds
		   (stage-dims (estate (entity-id :stage)))))))

(defvar* *update-rolling-average* (make-rolling-average (* *fps* 3)))
(defvar* *frame-rolling-average* (make-rolling-average (* *fps* 3)))
(defvar* *render-rolling-average* (make-rolling-average (* *fps* 3)))

(defun update-and-render! ()
  (rolling-average-time *update-rolling-average*
    (update-env! (update *env*)))

  (when (aval *env* :new-song)
    (switch-to-new-song! (aval *env* :new-song))
    (update-env! (arem *env* :new-song)))
  (when (aval *env* :sound-effects)
    (play-sounds! (aval *env* :sound-effects))
    (update-env! (arem *env* :sound-effects)))

  (rolling-average-time *render-rolling-average*
    (render! (nconc *debug-render-list* (aval *env* :render-list))
	     (current-camera-pos))))

(defun main! ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (let ((frame-timer 0)
	       (last-update-time))
	   (init!)
	   (setq last-update-time (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (let ((transient-input (gather-transient-input)))
		    (handle-debug-input! transient-input)
		    (update-env!
		     (aupdate *env* :input #_(gather-input _ transient-input))))

		  (when (>= frame-timer (* *update-period* *frame-time*))
		    (rolling-average-time *frame-rolling-average* (update-and-render!))
		    (setq frame-timer (- frame-timer
					 (* *update-period* *frame-time*))))

		  (let ((dt (- (sdl:get-ticks) last-update-time)))
		    ;; NOTE: if we are paused beyond our control, Don't play catchup.
		    (setq frame-timer
			  (+ frame-timer
			     (min dt (* 2 *frame-time*)))))
		  (setq last-update-time (sdl:get-ticks))
		  (music-update!)
		  (sdl:delay 1))))
      (cleanup!))))

(defun handle-input (env)
  "Handles input. Often called many times between updates.
This can be abused with the machine gun in TAS."

  (setq env (update-subsystem env :input #'update-input-entity))

  (let ((*env* env))
    (let ((input (aval env :input)))
      (when (or (joy-pressed? input :b) (key-pressed? input :x))
	;; Fire Gun
	(unless (dead? (estate (entity-id :player env)))
	  (setq env (update-world env (entity-id :player env) #'player-fire-gun))))))

  env)

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
     :top (create-rect (+ (make-v bs 0) pos)
		       (- size (make-v dbs bs)))

     :bottom (create-rect (+ (make-v bs bs) pos)
			  (- size (make-v dbs bs)))

     :left (create-rect (+ (make-v 0 bs) pos)
			(- size (make-v bs dbs)))

     :right (create-rect (+ (make-v bs bs) pos)
			 (- size (make-v bs dbs))))))

(defun rect->collision-rects (rect &optional (buffer-size 6))
  (collision-rects (rect-pos rect) (rect-size rect) buffer-size))

(defvar* *dorito-friction-acc* 0.00002)
(defvar* *dorito-bounce-speed* 0.225)

(defun make-pickup (&key type amt)
  (alist :type type :amt amt))

(defun dorito-fns-alist ()
  (alist :ai-fn #'pickup-ai
	 :draw-fn #'dorito-drawing
	 :stage-collision-fn #'dorito-stage-collision
	 :pickup-rect-fn #'dorito-pickup-rect))

(defvar* *pickup-subsystems*
    '(:timers :drawable :pickup))

(defun make-dorito (pos size)
  (let ((vel (polar-vec->v (rand-angle) 0.07)))
    (amerge
     (dorito-fns-alist)
     (alist :subsystems (list* :physics :stage-collision *pickup-subsystems*))
     (dorito-pickup-data size)
     (alist :timers
	    '(:life-timer :anim-cycle)
	    :id (gen-entity-id)
	    :life-timer
	    (make-expiring-timer (s->ms 8) t)
	    :anim-cycle
	    (make-fps-cycle 14 (iota 6))
	    :physics '(:stage-physics)
	    :stage-physics
	    (make-kin-2d :pos (- pos
				 (rect-pos (dorito-collision-rect size)))
			 :vel vel
			 :accelerator-x
			 (friction-accelerator *dorito-friction-acc*)
			 :accelerator-y
			 (const-accelerator *gravity-acc*)
			 :clamper-vy
			 (clamper+- *terminal-speed*))
	    :size size))))

(defun pickup-ai (d)
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
    (list
     (make-sprite-drawing
      :layer :pickup
      :sheet-key :npc-sym

      :src-rect
      (create-rect
       (+ (anim-cycle-offset d)
	  (tile-v 0 (1+ (position (aval d :size) '(:small :medium :large)))))
       (make-v (tiles 1) (1- (tiles 1))))

      :pos (physics-pos d)))))

(defun set-x-v (v x)
  (make-v x (y v)))

(defun set-y-v (v y)
  (make-v (x v) y))

(defun reverse-x-v (v)
  (make-v (- (x v)) (y v)))

(defun max-y-v (v max-y)
  (make-v (x v) (max (y v) max-y)))

(defvar* *dorito-stage-collisions*
    (alist
     :bottom
     (collision-lambda (data)
       (aupdate data
		:sound-effects
		(pushfn :snd-tink)
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

(setfn pickup-kill
       (lambda (pickup)
	 (aupdate pickup :sound-effects (pushfn (ecase (aval pickup :type)
						  (:heart :snd-health-refill)
						  (:dorito :snd-get-xp)
						  (:missile :snd-get-missile)))))
       (asetfn :dead? t))

(defun dorito-size->exp-amt (size)
  (ecase size
    (:small 1)
    (:medium 10)
    (:large 20)))

(defun dorito-pickup-data (size)
  (make-pickup :type :dorito
	       :amt (dorito-size->exp-amt size)))

(defun dorito-collision-rect (size)
  (centered-rect (tile-dims/2)
		 (ecase size
		   (:small (both-v (tiles 2/5)))
		   (:medium (both-v (tiles 3/4)))
		   (:large (both-v (tiles 1))))))

(defun single-loop-sprite-fns-alist ()
  (alist :ai-fn #'single-loop-sprite-ai))

(defvar* *single-loop-sprite-subsystems*
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
  (unless (dead? s)
    (list (make-sprite-drawing :layer (aval s :layer)
			       :sheet-key (aval s :sheet-key)
			       :src-rect
			       (tile-rect (tile-v (cycle-current
						   (aval s :anim-cycle))
						  (aval s :tile-y)))
			       :pos pos))))

(defun anim-cycle-idx (obj)
  (aval (aval obj :anim-cycle) :idx))

(defun single-loop-sprite-ai (p)
  (aset p
	:dead? (and (ticked? p :anim-cycle)
		    (zerop (anim-cycle-idx p)))))

(defun particle-fns-alist ()
  (alist :draw-fn #'particle-drawing
	 :ai-fn
	 ;; NOTE: This (ai/timers) is a kludge to make it so particle can set its
	 ;; dead? flag based on single-loop-sprite.
	 (lambda (p)
	   (aset p :dead? (dead? (estate (aval p :single-loop-sprite)))))))

(defvar* *particle-subsystems* '(:drawable :timers :ai))
(defun make-particle (&key seq fps sheet-key tile-y pos)
  (amerge
   (particle-fns-alist)
   (alist :subsystems *particle-subsystems*)
   (let ((sprite (make-single-loop-sprite fps seq sheet-key tile-y :particle)))
     (alist :single-loop-sprite (aval sprite :id)
	    :id (gen-entity-id)
	    :new-states (list sprite)
	    :pos pos))))

(defun particle-drawing (p)
  (let ((sp (estate (aval p :single-loop-sprite))))
    (unless (dead? sp)
      (list (make-sprite-drawing :layer :particle
				 :sheet-key (aval sp :sheet-key)
				 :src-rect
				 (tile-rect (tile-v (cycle-current
						     (aval sp :anim-cycle))
						    (aval sp :tile-y)))
				 :pos (aval p :pos))))))

(defun make-projectile-star-particle (center-pos)
  (make-particle :seq (alexandria:iota 4)
		 :fps 14
		 :sheet-key :caret
		 :tile-y 3
		 :pos (- center-pos (tile-dims/2))))

(defun make-projectile-wall-particle (center-pos)
  (make-particle :seq (mapcar (curry #'+ 11) (alexandria:iota 4))
		 :fps 14
		 :sheet-key :caret
		 :tile-y 0
		 :pos (- center-pos (tile-dims/2))))

(defun number-drawing
    (pos number &key (layer :hud) (centered? t) (show-sign? t))
  (let* ((neg? (minusp number))
	 (digits (fixnum->digits number))
	 (pos (if centered?
		  (- pos (tiles/2-v (/ (length digits) 2) 1/2))
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
	(setq pos (+ pos (tiles/2-v 1 0)))))
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

(defvar* *floating-number-subsystems*
    '(:timers :drawable :physics))
(defun make-floating-number (entity amt)
  (let ((entity (if (eq :exp entity)
		    (entity-id :player)
		    entity)))
    (amerge
     (floating-number-fns-alist)
     (alist :subsystems *floating-number-subsystems*)
     (alist :entity entity
	    :exp? (eq :exp entity)
	    :amt amt
	    :id (gen-entity-id)
	    :timers '(:life-timer)
	    :life-timer (make-expiring-timer (s->ms 2) t)
	    :physics '(:offset)
	    :origin (origin (estate entity))
	    :offset (make-offset-motion (zero-v)
					:up
					(/ (tiles 1/30) *frame-time*))))))

(defun floating-number-drawing (fn)
  (number-drawing (+ (aval fn :origin) (physics-pos fn))
		  (aval fn :amt)
		  :layer :floating-text))

(defun floating-number-ai (fn)
  
  (let ((dead? (not (timer-active? (aval fn :life-timer))))
	(offset
	 (if (< (y (motion-pos (aval fn :offset)))
		(- (tiles 1)))
	     (make-offset-motion (zero-v :y (- (tiles 1))) :up 0)
	     (aval fn :offset))))
    (aset fn
	  :dead? dead?
	  :offset offset
	  :origin
	  (let ((state (estate (aval fn :entity))))
	    (if (dead? state)
		(aval fn :origin)
		(origin state))))))

(defun floating-number-add-amt (fn amount)
  (aupdate fn
	   :life-timer #'reset-timer
	   :amt #_(+ _ amount)))

(defun remove-all-dead (env)
  (funcall (comp
	    #'entity-registry-remove-dead
	    #'registry-remove-dead
	    (lambda (env)
	      (let ((*env* env))
		(estate-set
		 env
		 (projectile-groups-remove-dead
		  (estate (entity-id :projectile-groups env) env)))))
	    (lambda (env)
	      (let ((*env* env))
		(estate-set
		 env
		 (damage-numbers-remove-dead
		  (estate (entity-id :damage-numbers env) env))))))
	   env))

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

(defvar* *text-speed* 100)
(defun slow-text-speed! ()
  (setq *text-speed* 100))
(defun fast-text-speed! ()
  (setq *text-speed* 25))
(defvar* *cursor-blink-time* 100)

#+nil
(defvar* *text-display-subsystems*
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
       (let ((char-dims (get-text-size (aval *sdl* :font) " "))
	     (w (x (get-text-size (aval *sdl* :fontn) (text-display-text td)))))
	 (draw-rect
	  (create-rect (+ (text-display-pos td) (make-v w 0)) char-dims)
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

(defvar* *hud-subsystems* '(:timers :drawable))
(defun make-hud ()
  (amerge
   (hud-fns-alist)
   (alist :subsystems *hud-subsystems*)
   (alist :id (gen-entity-id)
	  :persistence :indefinite
	  :timers '(:exp-change-timer :health-change-timer)
	  :exp-change-timer (make-expiring-timer (s->ms 1))
	  :health-change-timer (make-expiring-timer (s->ms 1/2)))))

(defun hud-drawing (hud)
  (unless (dead? (estate (entity-id :player)))
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

      (let ((health (aval (estate (entity-id :player)) :health-amt)))
	(when (timer-active? (aval hud :health-change-timer))
	  (push
	   (make-sprite-drawing
	    :layer :hud
	    :sheet-key :text-box
	    :src-rect
	    (create-rect-cmpts 0 (tiles/2 4)
			       (floor (* (tiles/2 bar-tile/2-w)
					 (/ (aval hud :last-health-amt)
					    (aval (estate (entity-id :player))
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
					  (aval (estate (entity-id :player))
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

	(multiple-value-bind (exp gun-name) (current-gun-exp (entity-id :player)
							     (entity-id :gun-exps))
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
      drawings)))

(setfn hud-exp-changed
       (aupdatefn :exp-change-timer #'reset-timer))

(setfn hud-health-changed
       (aupdatefn :health-change-timer #'reset-timer)
       (lambda (hud)
	 (aset hud
	       :last-health-amt
	       (aval (estate (entity-id :player)) :health-amt))))

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

(defun mouse->tile-pos (mouse-pos camera-pos)
  (pos->tile-pos (+ mouse-pos camera-pos)))

(defun update (env)
  "The Main Loop, called once per *FRAME-TIME*."
  (setq *debug-render-list* nil)
  (setq env (aset env :render-list nil))

  (when (eq *input-playback* :playback)
    (setq env (aset env :input (next-playback-input))))
  (unless (aval env :paused?)
    (setq env (handle-input env)))

  (case *input-playback*
    (:recording
     (draw-text-line (zero-v) "RECORD"))
    (:playback
     (draw-text-line (zero-v) "PLAYBACK")))

  (unless (aval env :paused?)
    (unless *stage-viewer*
      (setq env (update-subsystem env :timers #'update-timers-entity))
      (setq env (update-subsystem env :physics #'update-physics-entity))
      (setq env (update-subsystem env :bullet #'update-damageable-subsystem))
      (setq env (update-subsystem env :stage-collision #'update-stage-collision-entity))
      (setq env (update-subsystem env :pickup #'update-pickup-entity))
      (setq env (update-subsystem env :damage-collision #'update-damage-collision-entity))
      (setq env (update-subsystem env :dynamic-collision #'update-dynamic-collision-entity)))

    (setq env (remove-all-dead env)))

  (when (aval env :tsc-script)
    (setq env (run-tsc-script env)))
  (setq env (update-subsystem env :drawable #'update-drawable-entity)) 

  ;; Debug Drawings Below.

  (when (aval env :paused?)
    (draw-text-line (zero-v) "PAUSED"))

  (draw-text-line
   (make-v 0 (- (y *window-dims*) *tile-size*))
   (format nil "Renderer: ~,0f%"
	   (rolling-average-percent *render-rolling-average*)))
  (draw-text-line
   (make-v (* 6 *tile-size*) (- (y *window-dims*) *tile-size*))
   (format nil "Update: ~,0f%"
	   (rolling-average-percent *update-rolling-average*)))
  (draw-text-line
   (make-v (* 12 *tile-size*) (- (y *window-dims*) *tile-size*))
   (format nil "Total: ~,0f%"
	   (rolling-average-percent *frame-rolling-average*)))

  ;; (draw-point (player-nozzle-pos player) *red*)
  (let ((focus (camera-focus (estate (entity-id :camera))))
	(camera-bounds (stage-dims->camera-bounds
			(stage-dims (estate (entity-id :stage))))))
    (unless *stage-viewer*
      (draw-point focus *cyan*)
      (draw-point (clamp-pos focus camera-bounds) *red*))
    (draw-rect camera-bounds *cyan* :layer :debug-camera))
  (let ((player (estate (entity-id :player))))
    (unless (or *stage-viewer* (dead? player))
      (draw-point (camera-target-from-player player) *white*)))
  (let ((mouse-pos (input-mouse-coords (aval *env* :input))))
    (draw-point mouse-pos
		*white*
		:layer :mouse)
    (let ((tp (mouse->tile-pos mouse-pos (current-camera-pos))))
      (draw-text-line (make-v 320 0)
		      (format nil "TILE: [~A, ~A]"
			      (x tp) (y tp)))))

  ;; End Debug Drawings.
  (play-sounds! (aval env :sfx-play-list))
  (setq env (aset env :sfx-play-list nil))

  (when (eq *input-playback* :recording)
    (record-frame-input (aval *env* :input)))

  (aupdate env :input #'reset-transient-input))

(defvar* *input-playback* nil)

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

(defvar* *dialog-text-pos* (tiles/2-v 7 24))
(defvar* *entity-systems* '(:game :dialog))


(defun damage-numbers-remove-dead (d)
  (aset d
	:pairs
	(remove-if (lambda (pair) (dead? (estate (cdr pair)))) (aval d :pairs))))

(defun make-damage-numbers ()
  (alist :id (gen-entity-id)
	 :persistence :indefinite))

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
	 :persistence :indefinite
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
  (alist :id (gen-entity-id)
	 :persistence :indefinite))

(defun current-gun-exp (player gun-exps)
  (let* ((gun-name (player-current-gun-name (estate player)))
	 (exp (gun-exp-for (estate gun-exps) gun-name)))
    (values
     exp
     gun-name)))

(defun save-current-state ()
  (list *env* (aval *env* :game)))

(defun restore-state (state)
  (restore-entity-states! (first state))
  (update-env! (aset *env*
		     :registry nil
		     :game (second state))))

(defun stage-from-stage-name (stage-name)
  (stage-from-file-data
   (read-pxm-file (format nil "./content/stages/~A.pxm" stage-name))
   (read-pxa-file (format nil "./content/stages/~A.pxa" stage-name))))

(defun make-pxe-entity (entity game-flags)
  (let ((entity-flags (aval entity :flags))
	(type (aval entity :type)))
    (when (or (not (member :appear-on-flag-id entity-flags))
	      (member (aval entity :flag-id) game-flags))
      (let ((data (entity-npc-data type))
	    (e
	     (case type
	       (:bat-blue (make-bat (aval entity :tile-pos)))
	       (:critter-hopping-blue
		(make-critter (tile-pos (aval entity :tile-pos))))
	       (:door-enemy
		(make-door-enemy (aval entity :tile-pos)))
	       (:spike-small
		(make-spike (aval entity :tile-pos)
			    (case (aval entity :tsc-id)
			      (1 :up)
			      (2 :right)
			      (3 :down)
			      (t :left)))))))
	(when e
	  (amerge data e))))))

(defun make-pxe-entities (pxe flags)
  "Convert a parsed pxe list to a list of entities."
  (remove nil (mapcar #_(make-pxe-entity _ flags) pxe)))

(defun entities-from-stage-key (stage-key)
  (make-pxe-entities
   (read-pxe-file (format nil "./content/stages/~A.pxe"
			  (aval (aval *stage-fnames-table* stage-key)
				:entities)))
   nil))

(defun non-persistent-entities (&optional (env *env*))
  (remove-if #_(aval _ :persistence) (avals (aval env :entity-registry))))

(defun kill-entities (entities env)
  "Marks entities as dead."
  (dolist (e entities)
    (setq env (estate-set env (aset e :dead? t))))
  env)

(defun precompile-stage-drawings (stage)
  (aset stage :drawings (ncompile-drawings
			 (prerendered-stage-drawings
			  (aval stage :data)
			  (aval (aval *stage-fnames-table* (aval stage :stage-key)) :texture)))))

(defun switch-stage (env stage-key)
  (setq env (kill-entities (non-persistent-entities env) env))
  (mapc #_(release-resource :texture _)
	(mapcar (avalfn :texture) (aval (estate (entity-id :stage env) env) :drawings)))
  (setq env (estate-set env
			(precompile-stage-drawings
			 (load-stage-from-stage-key stage-key (entity-id :stage env)))))
  (setq env (estate-set env (aset (entity :damage-numbers) :pairs nil)))
  (setq env (estate-set env (aset (entity :projectile-groups) :groups nil)))
  (dolist (entity (entities-from-stage-key stage-key))
    (setq env (create-entity env (aval entity :id) entity)))
  env)

(defun tsc-transport (env args)
  "Moves the player to map-key, at the given tile-pos."
  (let ((map-key (aval args :map-key))
	(script-id (aval args :script-id))
	(player-pos (aval args :pos)))
    (setq env (switch-stage env map-key))
    (setq env (start-tsc-script env (get-stage-tsc-script (entity :stage env) script-id)))
    (setq env (estate-set env (set-player-pos (entity :player env) player-pos)))
    (estate-set env (set-camera-focus (entity :camera env) player-pos))))

(defun tsc-change-music (env args)
  "Change the song to song-key."
  (aset env :new-song (aval args :song-key)))

(defun tsc-sound-effect (env args)
  "Play sound-effect-key"
  (aupdate env :sound-effects (pushfn (aval args :sound-effect-key))))

(defun interpret-tsc-command (env command)
  "Interpret the command, returning a new environment."
  (if (fboundp (aval command :fn))
      (funcall (aval command :fn) env (aval command :args))
      (progn
	(warn "Ignoring command ~A" command)
	env)))

(defun start-tsc-script (env script)
  (aset env :tsc-script script))

(defun run-tsc-script (env)
  (loop while (and (aval env :tsc-script)
		   (not (aval env :tsc-command)))
     do
       (setq env (interpret-tsc-command env (first (aval env :tsc-script))))
       (setq env (aupdate env :tsc-script #'rest)))
  env)

(defun entity (key &optional (env *env*))
  (estate (entity-id key env) env))

(defun transport-to-cave! ()
  "Moves the  player to the first cave."
  (update-env!
   (funcall
    (comp
     #_(interpret-tsc-command _ `((:fn . tsc-sound-effect) (:ARGS . ,(alist :sound-effect-key :snd-door))))
     #_(interpret-tsc-command _ `((:fn . tsc-change-music) (:ARGS . ,(alist :song-key :song-gestation))))
     #_(interpret-tsc-command _ `((:fn . tsc-transport) (:ARGS . ,(alist :map-key :cave :pos (tile-v 37 11))))))
    *env*)))

(defun transport-to-pole! ()
  "Moves the player to the Hermit's house."
  (setq *env*
	(interpret-tsc-command *env* `((:fn . tsc-transport) (:ARGS . ,(alist :map-key :pole :script-id 92 :pos (tile-v 7 9)))))))

(defun create-game (env)
  (let* ((stage-key :cave)
	 (damage-numbers (make-damage-numbers))
	 (projectile-groups (make-projectile-groups))

	 (stage (precompile-stage-drawings
		 (load-stage-from-stage-key stage-key)))
	 (entities (entities-from-stage-key stage-key))
	 (hud (make-hud))
	 (player (make-player :pos (tile-v 37 10)))
	 (gun-exps (make-gun-exps)))
    
    (let ((camera (make-camera (physics-pos player) (zero-v) player)))
      (mapc (lambda (entity)
	      (setq env (create-entity env (aval entity :id) entity)))
	    (list*
	     stage
	     projectile-groups
	     damage-numbers
	     stage
	     
	     hud
	     player
	     gun-exps
	     camera
	     entities))

      (aset env
	    :input (make-input)
	    :game
	    (make-game :player (aval player :id)
		       :camera (aval camera :id)
		       :stage (aval stage :id)
		       :hud (aval hud :id)
		       :projectile-groups (aval projectile-groups :id)
		       :gun-exps (aval gun-exps :id)
		       :damage-numbers (aval damage-numbers :id))))))

(defun reset! ()
  (switch-to-new-song! :lastcave)
  (set-music-volume! 20)
  (update-env! (funcall (comp (asetfn :paused? nil)
			      create-game
			      init-entity-registry) *env*)))

(defun init! ()
  "Called at application startup."
  (setq *update-rolling-average* (make-rolling-average (* *fps* 3)))
  (setq *render-rolling-average* (make-rolling-average (* *fps* 3)))
  (setq *frame-rolling-average* (make-rolling-average (* *fps* 3)))

  (sdl:init '(:audio :video :joystick))
  (sdl.ttf:init)
  (update-env! (alist :font (sdl.ttf:open-font "./content/VeraMoBd.ttf" 19)))

  (sdl.mixer:open-audio sdl.mixer:+default-frequency+
			sdl.mixer:+default-format+
			2
			4096)

  (update-env! (init-input *env*))
  (sdl:show-cursor :disable)
  
  (multiple-value-bind (window renderer)
      (sdl:default-window-and-renderer
	  "Cave Story"
	  (x *window-dims*) (y *window-dims*)
	  '()
	  '(:target-texture))
    (update-env! (aset *env*
		       :window window
		       :renderer renderer
		       :current-song nil)))
  (sdl:set-render-draw-blend-mode (aval *env* :renderer) :blend)
  (reset!))

(defun cleanup! ()
  "Called at application closing to cleanup all subsystems."
  (cleanup-input!)
  ;; Cleanup stage drawings
  (mapcar
   (lambda (texture) (release-resource :texture texture))
   (mapcar #_ (aval _ :texture) (aval (estate (entity-id :stage)) :drawings)))
  (cleanup-all-resources!)
  (clrhash *character-textures*)

  (setq *input-playback* nil)
  (sdl.mixer:close-audio)
  (sdl:destroy-renderer (aval *env* :renderer))
  (sdl:destroy-window (aval *env* :window))
  (sdl.ttf:quit)
  (sdl:quit)
  (update-env! (aset *env*
		     :renderer nil
		     :window nil
		     :font nil
		     :event nil)))

(defun quit ()
  "Quits the application."
  (throw 'exit nil))

(defun face-player (pos player)
  (if (< (x pos) (x (physics-pos (estate player)))) :right :left))

(defun bat-fns-alist ()
  (alist :damageable-hit-react-fn #'bat-hit-react
	 :origin-fn #'physics-tile-origin
	 :draw-fn #'bat-drawing
	 :ai-fn #'bat-ai
	 :damage-collision-rect-fn
	 (comp point-rect origin)
	 :damage-collision-amt-fn (constantly 1)
	 :damageable-rect-fn #'physics-tile-rect))

(defvar* *bat-subsystems*
    '(:timers :damage-collision :damageable :drawable :physics))

(defun make-bat (tile-pos)
  (amerge
   (bat-fns-alist)
   (alist :subsystems *bat-subsystems*)
   (alist
    :id (gen-entity-id)
    :physics '(:wave)
    :wave (make-wave-motion
	   :origin (tile-pos tile-pos)
	   :dir :up
	   :amp (tiles 2)
	   :speed (/ 0.0325
		     *frame-time*))
    :timers '(:anim-cycle)
    :anim-cycle (make-fps-cycle 14 #(0 2 1 2)))))

(defun bat-drawing (b)
  (list (make-sprite-drawing :layer :enemy
			     :sheet-key :npc-cemet
			     :src-rect
			     (tile-rect (+ (tile-v 2 2)
					   (anim-cycle-offset b)
					   (facing-offset b)))
			     :pos (physics-pos b))))

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
			(dead? (estate (aval p :single-loop-sprite)))))))

(defvar* *death-cloud-particle-subsystems*
    '(:drawable :physics :stage-collision :timers :ai))

(defun make-death-cloud-particle (pos)
  (amerge
   (death-cloud-particle-fns-alist)
   (alist :subsystems *death-cloud-particle-subsystems*)
   (let ((sprite (make-single-loop-sprite
		  15 (mapcar #'1+ (alexandria.0.dev:iota 7))
		  :npc-sym 0 :particle)))
     (alist :single-loop-sprite (aval sprite :id)
	    :new-states (list sprite)
	    :physics '(:stage-physics)
	    :id (gen-entity-id)
	    :stage-physics
	    (make-kin-2d
	     :pos (- pos (tile-dims/2))
	     :vel (polar-vec->v (rand-angle)
				(rand-val-between 0.1 0.3))
	     :clamper-vx
	     (clamper+- *terminal-speed*)
	     :clamper-vy
	     (clamper+- *terminal-speed*))))))

(defun death-cloud-particle-drawing (d)
  (let ((state (estate (aval d :single-loop-sprite))))
    (unless (dead? state)
      (single-loop-sprite-drawing state (physics-pos d)))))

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
  (collect #_(make-death-cloud-particle pos) num))

(defvar* *critter-dynamic-collision-rect*
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

(defun make-critter (pos)
  (let ((id (gen-entity-id)))
    (amerge
     (critter-fns-alist)
     (alist :subsystems *critter-subsystems*)
     (alist
      :stage-physics (gravity-kin-2d :pos pos)
      :physics '(:stage-physics)
      :player-origin (zero-v)
      :id id))))

(defvar* *critter-subsystems*
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
  (aset obj :facing (face-player (physics-pos obj)
				 (entity-id :player))))

(defun critter-jump-ai (c)
  (setq c (aset c
		:player-origin
		(let ((state (estate (entity-id :player))))
		  (if state
		      (origin state)
		      (aval c :player-origin)))))
  (let ((facing (aval c :facing)))

    (if (and (not (timer-active? (aval c :sleep-timer)))
	     (< (dist (origin c) (aval c :player-origin)) (tiles 4))
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
			  ((< (dist (origin c) (aval c :player-origin))
			      (tiles 7))
			   1)
			  (t
			   0))))
    (list
     (make-sprite-drawing :layer :enemy
			  :sheet-key :npc-cemet
			  :src-rect (tile-rect (+ (tile-v sprite-tile-x 0)
						  (facing-offset c)))
			  :pos (physics-pos c)))))



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
			 :pos (-
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

(defvar* *critter-stage-collisions*
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
  (+ (physics-pos c) (tile-dims/2)))
(defun physics-tile-rect (c)
  (tile-rect (physics-pos c)))

(defun stage-vel (e)
  (aval (aval e :stage-physics) :vel))

(defun critter-inertia-vel (c)
  (stage-vel c))

(defvar* *elephant-speed* 0.08)

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

(defvar* *elephant-subsystems*
    '(:timers :drawable :physics :stage-collision
      :damageable :damage-collision :dynamic-collision))

(defun make-elephant (pos)
  (amerge
   (elephant-fns-alist)
   (alist :subsystems *elephant-subsystems*)
   (alist :stage-physics
	  (gravity-kin-2d :pos pos
			  :vel (make-v (- *elephant-speed*) 0))
	  :physics '(:stage-physics)
	  :timers '(:anim-cycle)
	  :anim-cycle (make-fps-cycle 12 #(0 2 4))
	  :facing :left)))

(defvar* *elephant-dims* (make-v (tiles 2) (tiles 3/2)))
(defun elephant-drawing (e)
  (let ((src-pos
	 (cond
	   ((timer-active? (aval e :recover-timer))
	    (make-v (tiles (* 2 3)) 0))
	   (t (anim-cycle-offset e)))))
    (list
     (make-sprite-drawing
      :layer :enemy
      :sheet-key :npc-eggs1
      :src-rect
      (create-rect (+ src-pos
		      (make-v 0 (if (eq (aval e :facing) :left)
				    0
				    (tiles 3/2))))
		   *elephant-dims*)
      :pos (physics-pos e)))))

(defun elephant-origin (e)
  (+ (physics-pos e)
     (* *elephant-dims* 1/2)))

(defun elephant-inertia-vel (e)
  (* (stage-vel e) 1/3))

(defun elephant-damageable-rect (e)
  (create-rect (physics-pos e) *elephant-dims*))

(setfn shake-hit-react
       (aupdatefn
	:timers (adjoinfn :shake-timer)
	:physics (adjoinfn :shake))
       (asetfn
	:shake-timer
	(make-expiring-timer (s->ms 1/3) t)
	:shake
	(make-wave-motion :dir :left :amp 2 :speed 0.1 :rads 0)))

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

(defun floating-number-update (id amt)
  "Returns a list of new states for entities."
  (let* ((d (estate (entity-id :damage-numbers)))
	 (dns (aval d :pairs))
	 (existing-dn-pair (assoc id dns)))
    (if existing-dn-pair
	(list (floating-number-add-amt
	       (estate (cdr existing-dn-pair)) amt))
	(let ((fn (make-floating-number id amt)))
	  (list (aupdate d :pairs
			 (pushfn (cons id (aval fn :id))))
		fn)))))

(defun damage-number-update (id amt)
  (floating-number-update id (- amt)))

(defun damage-number-update-amtfn (amt)
  (lambda (obj)
    (aupdate obj
	     :new-states
	     (appendfn (damage-number-update (aval obj :id) amt)))))

(defun experience-number-update (id amt)
  (floating-number-update id amt))

(defun make-drops (origin amt)
  "Makes the equivalent number of pickups to amt from origin."
  ;; 3/5 of the time choose doritos, otherwise missile or heart.
  (let ((bundle? (> amt 6)))
    (case (random 4)
      (0 ;;Missile
       (list (make-missile-pickup origin bundle?)))
      (1 ;;Heart
       (list (make-heart-pickup origin bundle?)))
      (t
       (let* ((large (dorito-size->exp-amt :large))
	      (medium (dorito-size->exp-amt :medium))
	      (small (dorito-size->exp-amt :small))
	      (num-large (floor amt large))
	      (num-medium (floor (- amt (* large num-large)) medium))
	      (num-small (floor (- amt (* large num-large) (* medium num-medium))
				small)))
	 (nconc
	  (collect #_(make-dorito origin :large)
		   num-large)
	  (collect #_(make-dorito origin :medium)
		   num-medium)
	  (collect #_(make-dorito origin :small)
		   num-small)))))))

(defun damage-reaction (obj)
  (let ((amt (aval obj :damage-amt)))
    (funcall
     (if (< amt (aval obj :health-amt))
	 (comp
	  (damage-number-update-amtfn amt)
	  (aupdatefn
	   :health-amt #_(- _ amt)
	   :sound-effects (pushfn :snd-enemy-hurt)))
	 (let ((origin (origin obj)))
	   (comp
	    (asetfn :dead? t)
	    (aupdatefn
	     :new-states
	     (appendfn
	      (make-drops origin (aval obj :exp-for-kill))
	      (make-num-death-cloud-particles (aval obj :smoke-amt)
					      origin))
	     :sound-effects (pushfn :snd-little-crash)))))
     obj)))

(defun elephant-dynamic-collision-rect (e)
  (let ((pos (physics-pos e)))
    (cond
      ((timer-active? (aval e :recover-timer))
       (setq pos (+ pos (make-v 0 (tiles 1/4)))))
      ((= 1 (anim-cycle-idx e))
       (setq pos (+ pos (make-v 0 (tiles 1/8))))))
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
    (create-rect (+ (physics-pos e) pos) dims)))

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
       (aupdatefn
	:timers (removefn :rage-timer))
       (asetfn
	:anim-cycle (make-fps-cycle 12 #(0 2 4))
	:rage-timer nil))

(setfn elephant-rage-begin
       (aupdatefn
	:timers (adjoinfn :rage-timer)
	:rage-timer #'reset-timer)
       (asetfn
	:anim-cycle (make-fps-cycle 14 #(8 10))))

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
	       :sound-effects (pushfn :snd-thud)
	       :new-states
	       (appendfn
		(list*
		 (timed-camera-shake (estate (entity-id :camera))
				     (s->ms 1/2))
		 (make-num-death-cloud-particles
		  3 (+ (physics-pos e)
		       (make-v (if (eq (aval e :facing) :left)
				   (tiles 3/2)
				   (tiles 1/2))
			       (tiles 5/4)))))))
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

(setfn bat-hit-react damage-reaction)
(setfn bat-ai face-player-ai)
(setfn critter-ai
       face-player-ai
       critter-jump-ai
       shake-ai)
(setfn critter-hit-react
       damage-reaction shake-hit-react)
(setfn elephant-hit-react 
       damage-reaction
       elephant-rage-hit-react
       shake-hit-react)
(setfn elephant-ai
       elephant-stage-physics-ai
       elephant-rage-effects
       elephant-rage-ai
       shake-ai)

(defun read-uint8 (in)
  (read-byte in))

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
    ;; Discard PXM,16
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

(defun read-pxe-file (path)
  (remove-if-not
   #'valid-entity
   (mapcar
    (comp
     (lambda (e)
       (apply #'arem e (remove-if #_(aval e _) '(:type :flags))))
     (aupdatefn
      :type #_(aref *entity-type-table* _)
      :flags #'pxe-flags->entity-flags)
     #'remove-entity-zero-flags)
    (parse-pxe-file path))))

(defun parse-pxe-file (path)
  "Parses a pxe entity file."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    ;; Discard PXE,16
    (dotimes (i 4) (read-byte stream))
    (let ((num-entities (read-uint32 stream))
	  entities)
      (dotimes (i num-entities)
	(push (alist :tile-pos (make-v (read-uint16 stream)
				       (read-uint16 stream))
		     :flag-id (read-uint16 stream)
		     :tsc-id (read-uint16 stream)
		     :type (read-uint16 stream)
		     :flags (read-uint16 stream))
	      entities))
      entities)))

(defun valid-entity (entity)
  (some #_(aval entity _) '(:flag-id :tsc-id :type :flags)))

(defun remove-entity-zero-flags (e)
  (apply #'arem
	 e
	 (remove-if-not (lambda (k) (zerop (aval e k)))
			'(:flag-id :tsc-id))))

(defun pxe-flags->entity-flags (flags)
  "Takes an integer flags and parses it into a set of entity-flags."
  (mapcar #'car
	  (remove-if (lambda (f) (zerop (logand flags (second f))))
		     *entity-flags*)))

(defun read-pxa-file (path)
  (read-file-into-byte-vector path))

(defun tile-attribute-num->tile-attributes (num)
  (let ((attrs (elt *tile-attributes-table* num)))
    (if (member :slope attrs)
	(let ((slope-idx (cond ((>= num 112)
				(- num 112))
			       (t (- num #x50)))))
	  (cons (elt '(:ltt :lts :rts :rtt :lbt :lbs :rbs :rbt)
		     slope-idx)
		attrs))
	attrs)))

(defun pxm-tile-offset-idx->tile-v (idx)
  "Get the tile position given an index into the pxa array."
  (make-v (mod idx 16) (floor idx 16)))

(defun pxm-and-attrs->stage-data (pxm attrs)
  "Create stage-data from a list of row-major tiles, and a vector of
tile attribute lists."
  (let ((width (aval pxm :width))
	(height (aval pxm :height))
	(tile-offset-idxs (aval pxm :tile-offset-idxs)))
    (let ((stage (make-array (list height width))))
      (loop for y from 0 below height do
	   (loop for x from 0 below width do
		(setf (aref stage y x)
		      (list (elt attrs (car tile-offset-idxs))
			    (pxm-tile-offset-idx->tile-v
			     (car tile-offset-idxs))))
		(setq tile-offset-idxs
		      (cdr tile-offset-idxs))))
      stage)))

(defun stage-data-from-file-data (pxm pxa)
  "Create a stage given the file data."
  (pxm-and-attrs->stage-data pxm
			     (map 'vector #'tile-attribute-num->tile-attributes pxa)))

(defun read-tsc-file (path)
  "Load/decrypt/parse a tsc file given a path."
  (parse-decrypted-tsc-script (decrypt-tsc-file path)))

(defun decrypt-tsc-file (path)
  "Loads in and decrypts a tsc script file. Returns a string."
  (let* ((bytes (read-file-into-byte-vector path))
	 (key-idx (floor (length bytes) 2))
	 (key (aref bytes key-idx)))
    ;; The middle character is the "key".
    ;; All other bytes have the key subtracted from it.
    (loop
       for i from 0
       for byte across bytes
       unless (= i key-idx) do
	 (setf (aref bytes i) (- byte key)))
    ;; Remove Carriage Return.
    (remove #\return (map 'string #'code-char bytes))))

(defun parse-decrypted-tsc-script (script)
  "Given the decrypted tsc script, return a parsed result."
  (let ((parsed-commands
	 (mapcan #'parse-tsc-line
		 (remove-if (lambda (d) (zerop (length d)))
			    (split-sequence:split-sequence #\newline script)))))
    (let (scripts
	  script)
      (loop for pc in parsed-commands
	 do
	   (when (and (consp pc) (eq (car pc) :id))
	     (push (nreverse script) scripts)
	     (setq script nil))
	   (push pc script))
      (push (nreverse script) scripts)
      (remove nil (nreverse scripts)))))

(defun rest-of-line (line start)
  (when (>= (length line) start)
    (subseq line start)))

(defun make-text-command (text)
  (alist :description "Displays text."
	 :args (alist :text text)
	 :fn 'tsc-display-text))

(defun parse-tsc-line (line)
  (if (zerop (length line))
      nil
      (case (aref line 0)
	(#\# (list (cons :id (car (parse-tsc-num (subseq line 1))))))
	(#\< (let ((parse (parse-tsc-command line))) 
	       (cons (car parse) (parse-tsc-line (cdr parse)))))
	(t (let ((parse (parse-tsc-text line)))
	     (cons (make-text-command (car parse)) (parse-tsc-line (cdr parse))))))))

(defun parse-tsc-text (line)
  "Assumes line starts with text for a message.
Parses up to the next <."
  (let ((text (first (split-sequence:split-sequence #\< line))))
    (cons text (rest-of-line line (length text)))))

(defun lookup-tsc-command (cmd-string)
  "Finds the tsc command associated with cmd-string."
  (find cmd-string *tsc-command-table*
	:key #_ (aval _ :command)
	:test #'string=))

(defun interpret-tsc-command-args (cmd arg-vals)
  (let ((args (pairlis (if (aval cmd :arg-keys)
			   (aval cmd :arg-keys)
			   (subseq '(:x :y :z :w) 0 (length arg-vals)))
		       arg-vals)))
    (when (aval args :map-num)
      (setq args (aset args :map-key (map-idx->map-keyword (aval args :map-num)))))
    (when (aval args :song-num)
      (setq args (aset args :song-key (song-idx->song-key (aval args :song-num)))))
    (when (aval args :sound-effect-num)
      (setq args (aset args :sound-effect-key (sound-effect-idx->sound-effect-key (aval args :sound-effect-num)))))
    (when (aval args :dir-num)
      (setq args (aset args :dir-key (aref *directions-table* (aval args :dir-num)))))
    (when (and (aval args :tile-x) (aval args :tile-y))
      (setq args (aset args :pos (tile-v (aval args :tile-x) (aval args :tile-y)))))
    args))

(defun parse-tsc-command (line)
  "Assumes line starts with a tsc command <XYX0001:1234...
Returns (command . remaining-line)"
  (let* ((cmd-string (subseq line 1 4))
	 (cmd (lookup-tsc-command cmd-string)))
    (if (and (> (length line) 4) (digit-char-p (aref line 4)))
	(let ((parse (parse-tsc-command-args (subseq line 4))))
	  (cons (aset cmd :args (interpret-tsc-command-args cmd (car parse)))
		(cdr parse)))
	(cons cmd
	      (rest-of-line line 4)))))

(defun parse-tsc-command-args (line)
  "Assumes line is 0000:1234:5678..."
  (let* ((parse (parse-tsc-num line))
	 (arg (car parse))
	 (line2 (cdr parse)))
    (if (zerop (length line2))
	(cons (list arg) line2)
	(case (aref line2 0)
	  (#\: (let ((parse2 (parse-tsc-command-args (subseq line2 1))))
		 (cons (cons arg (car parse2))
		       (cdr parse2))))
	  (t (cons (list arg) line2))))))

(defun parse-tsc-num (line)
  "Assumes line starts with a tsc num 0000...
Returns (num . remaining-line)."
  (let ((num (parse-integer line :junk-allowed t)))
    (cons num (rest-of-line line 4))))

(defun collect (op count)
  "Collects the results of calling op count times."
  (loop for i from 1 to count collecting (funcall op)))

(defun read-npc-table (path)
  "Reads the npc.tbl file. Returns parallel lists, indexed by
the entity type."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((count 361))
      (alist
       :default-flags (collect #_(read-uint16 stream) count)
       :health-amt (collect #_ (read-uint16 stream) count)
       :spritesheet-num (collect #_ (read-uint8 stream) count)
       :death-sound (collect #_ (read-uint8 stream) count)
       :hurt-sound (collect #_ (read-uint8 stream) count)
       :smoke-amt (collect #_ (read-uint8 stream) count)
       :exp-for-kill (collect #_ (read-uint32 stream) count)
       :damage (collect #_ (read-uint32 stream) count)))))

(defvar* *entity-flags*
    '((:solid-mushy 		#x0001 "Pushes player out, but is not solid. Normal State for enemies.")
      (:ignore-tile		#x0002)
      (:invulnerable		#x0004 "Invulnerable. Plays clinking sound when shot.")
      (:ignore-solid		#x0008)
      (:bouncy			#x0010 "When :solid-brick is set, acts like a mini-trampoline")
      (:shootable		#x0020)
      (:solid-brick		#x0040 "Bounding box is solid, just like a solid tile.")
      (:no-rear-top-attack	#x0080 "When attacked from the rear or top, damage is 0.")
      (:script-on-touch		#x0100 "Activate tsc-id script when the player touches this entity.")
      (:script-on-death		#x0200 "Activate tsc-id script when the entity dies.")
      (:drop-powerups-dont-use	#x0400 "unused.")
      (:appear-on-flag-id	#x0800 "This entity should spawn if the flag-id is set")
      (:faces-right		#x1000 "Sets the initial direction the enemy is facing to be to the right.")
      (:script-on-activate	#x2000 "Activate tsc-id script when the player interacts with this entity.")
      (:disappear-on-flag-id	#x4000 "This entity should NOT spawn if the flag-id is set.")
      (:show-float-text		#x8000 "This enemy should have a floating-text associated with it."))
  "Flags that represent the entity's attributes. Set by npc.tbl and .tsc files.")

(defvar* *tile-attribute-flags*
    '((:solid-player	#x001 "Solid to the player.")
      (:solid-npc	#x002 "Solid to NPCs.")
      (:solid-shot	#x004 "Solid to bullets.")
      (:hurts-player	#x010 "Causes 10HP of damage to player.")
      (:foreground	#x020 "Drawn on the foreground layer.")
      (:destroyable	#x040 "Destroyable purple star block.")
      (:water		#x080 "Tile is underwater.")
      (:current		#x100 "Tile has a water/wind current.")
      (:slope		#x200 "Tile is sloped."))
  "Tile attribute flags for pxa file. Numbers correspond to the tilekey.dat
file from nx engine.")

(defun read-tile-key-table ()
  "Read the tilekey.dat file (from nx) to generate tile-attributes-table."
  (mapcar (lambda (num)
	    (mapcar #'first (remove-if
			     (lambda (attr)
			       (zerop (logand num (second attr))))
			     *tile-attribute-flags*)))
	  (with-open-file (stream "./content/tilekey.dat"
				  :element-type '(unsigned-byte 8))
	    (loop for i from 1 to 256
	       collecting (read-uint32 stream)))))

(defvar* *tile-attributes-table*
    (map 'vector #'identity (read-tile-key-table))
  "Table of tile-attribute-idx (from a .pxa file) to a list
of *tile-attributes*.")

(defvar* *entity-type-table*
    #(nil
      :xp
      :behemoth
      nil
      :smoke-cloud
      :critter-hopping-green
      :beetle-green
      :basil
      :beetle-freefly
      :balrog-drop-in
      nil
      :igor-shot
      :balrog
      :forcefield
      :santas-key
      :chest-closed
      :save-point
      :recharge
      :door
      :balrog-bust-in
      :computer
      :chest-open
      :teleporter
      :teleporter-lights
      :power-critter
      :egg-elevator
      :bat-circle
      :big-spike
      :critter-flying
      :chthulu
      :hermit-gunsmith
      :bat-hang
      :life-capsule
      :balrog-shot-bounce
      :bed
      :mannan
      :balrog-boss-flying
      :signpost
      :fireplace
      :save-sign
      :santa
      :door-busted
      :sue
      :chalkboard
      :polish
      :polishbaby
      :hvtrigger
      :sandcroc
      nil
      :skullhead
      :skeleton-shot
      :crowwithskull
      :blue-robot-sitting
      :skullstep-foot
      :skullstep
      :kazuma
      :beetle-brown
      :crow
      :giant-beetle
      :door-enemy
      :toroko
      :king
      :kazuma-at-computer
      :toroko-shack
      :critter-hopping-blue
      :bat-blue
      :miserys-bubble
      :misery-float
      :balrog-boss-running
      :mushroom-enemy
      :hidden-sparkle
      :chinfish
      :sprinkler
      :water-droplet
      :jack
      :kanpachi-fishing
      :yamashita-flowers
      :yamashita-pavilion
      :pot
      :mahin
      :gravekeeper
      :giant-mushroom-enemy
      :misery-stand
      :npc-igor
      :giant-beetle-shot
      :terminal
      :missile
      :heart
      :boss-igor
      :boss-igor-defeated
      nil
      :cage
      :sue-at-computer
      :chaco
      :giant-jelly
      :jelly
      :fan-left
      :fan-up
      :fan-right
      :fan-down
      :grate
      :powercomp
      :powersine
      :mannan-shot
      :frog
      :hey
      :hey-spawner
      :malco
      :balfrog-shot
      :malco-broken
      :minifrog
      :ptelout
      :ptelin
      :professor-booster
      :press
      :frenzied-mimiga
      :red-petals
      :curly
      :curly-boss
      :tablechairs
      :mimigac1
      :mimigac2
      :mimigac-enemy
      :curlyboss-shot
      :sunstone
      :hidden-powerup
      :puppy-run
      nil
      nil
      nil
      :puppy-wag
      :puppy-sleep
      :puppy-bark
      :jenka
      :armadillo
      :skeleton
      :puppy-carry
      :largedoor-frame
      :largedoor
      :doctor
      :toroko-frenzied
      :toroko-block
      :toroko-flower
      :jenka-collapsed
      :toroko-teleport-in
      :kings-sword
      :lightning
      :critter-shooting-purple
      :critter-shot
      :block-moveh
      :npc-player
      :blue-robot
      :shutter-stuck
      :gaudi
      :gaudi-dying
      :gaudi-flying
      :gaudi-flying-shot
      :block-movev
      :x-fishy-missile
      :x-defeated
      :pooh-black
      :pooh-black-bubble
      :pooh-black-dying
      :dr-gero
      :nurse-hasumi
      :curly-collapsed
      :gaudi-shopkeep
      :booster-falling
      :boulder
      :balrog-boss-missiles
      :balrog-missile
      :firewhirr
      :firewhirr-shot
      :gaudi-armored
      :gaudi-armored-shot
      :gaudi-egg
      :buyobuyo-base
      :buyobuyo
      :minicore-shot
      :core-ghostie
      :curly-ai
      :cai-gun
      :cai-mgun
      :cai-watershield
      :shutter-big
      :shutter
      :almond-lift
      :fuzz-core
      :fuzz
      nil
      :almond-robot
      :waterlevel
      :motorbike
      :motorbike-broken
      :blue-robot-remains
      :grating
      :motion-wall
      :ironh-fishy
      :ironh-shot
      :fan-droplet
      :dragon-zombie
      :dragon-zombie-dead
      :dragon-zombie-shot
      :critter-hopping-aqua
      :falling-spike-small
      :falling-spike-large
      :counter-bomb
      :counter-bomb-number
      :giant-beetle-2
      nil
      :beetle-freefly-2
      :spike-small
      :sky-dragon
      :night-spirit
      :night-spirit-shot
      :sandcroc-oside
      :pixel-cat
      :itoh
      :core-blast
      :bubble-spawner
      :mimiga-farmer-standing
      :mimiga-farmer-walking
      :jail-grating
      :momorin
      :chie
      :megane
      :kanpachi-standing
      :bucket
      :droll-guard
      :red-flowers-sprouts
      :red-flowers-blooming
      :rocket
      :orangebell
      :orangebell-baby
      :flowers-pens1
      :midorin
      :gunfish
      :gunfish-shot
      :proximity-press-hoz
      :mimiga-cage
      :mimiga-jailed
      :critter-hopping-red
      :red-bat
      :red-bat-spawner
      :lava-drip
      :lava-drip-spawner
      :proximity-press-vert
      :boss-misery
      :misery-shot
      :misery-phase
      :misery-ball
      :black-lightning
      :misery-ring
      :xp-capsule
      :helicopter
      :helicopter-blade
      :doctor-crowned
      :red-crystal
      :mimiga-sleeping
      :curly-carried
      :mimiga-caged
      :chie-caged
      :chaco-caged
      :boss-doctor
      :doctor-shot
      :doctor-shot-trail
      :doctor-blast
      :boss-doctor-frenzied
      :igor-balcony
      :doctor-bat
      :red-energy
      :ironh-brick
      :brick-spawner
      :droll-shot
      :droll
      :puppy-items
      :red-demon
      :red-demon-shot
      :little-family
      :falling-block
      :sue-teleport-in
      :doctor-ghost
      :udmini-platform
      :misery-frenzied
      :sue-frenzied
      :ud-spinner
      :ud-spinner-trail
      :ud-smoke
      :ud-pellet
      :misery-critter
      :misery-bat
      :ud-minicore-idle
      :quake
      :ud-blast
      :falling-block-spawner
      :cloud
      :cloud-spawner
      nil
      :intro-doctor
      :intro-kings
      :intro-crown
      :misery-missile
      :scroll-controller
      nil
      :gaudi-patient
      :baby-puppy
      :balrog-medic
      :santa-caged
      :stumpy
      :bute-flying
      :bute-sword
      :bute-archer
      :bute-arrow
      :ma-pignon
      :ma-pignon-rock
      :ma-pignon-clone
      :bute-dying
      :mesa
      :mesa-dying
      :mesa-block
      :curly-carried-shooting
      :ccs-gun
      :deleet
      :bute-falling
      :bute-spawner
      :hp-lightning
      :turning-human
      :ahchoo
      :transmogrifier
      :building-fan
      :rolling
      :ballos-bone
      :ballos-bone-spawner
      :ballos-target
      :straining
      :ikachan
      :ikachan-spawner
      :numahachi
      :green-devil
      :green-devil-spawner
      :ballos-priest
      :ballos-smile
      :ballos-rotator
      :ballos-body-2
      :ballos-eye-2
      :ballos-skull
      :ballos-platform
      :hoppy
      :ballos-spikes
      :statue-base
      :bute-archer-red
      :statue
      :the-cast
      :bute-sword-red
      :wall-collapser
      :balrog-passenger
      :balrog-flying
      :puppy-ghost
      :misery-wind
      :droplet-spawner
      :thank-you
      nil
      nil
      :balfrog
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      :player
      :heart3
      :missile3
      :lava-droplet
      :skullhead-carried
      :bbox-puppet
      :smoke-dropper
      nil
      nil
      nil
      :core-controller
      :core-front
      :core-back
      :core-marker
      :minicore
      nil
      nil
      nil
      nil
      nil
      :polar-shot
      :mgun-spawner
      :mgun-leader
      :mgun-trail
      :mgun-l1-shot
      :missile-shot
      :supermissile-shot
      :missile-boom-spawner
      :fireball1
      :fireball23
      :fireball-trail
      :blade12-shot
      :blade3-shot
      :blade-slash
      :snake1-shot
      :snake23-shot
      :snake-trail
      :nemesis-shot
      :nemesis-shot-curly
      :bubbler12-shot
      :bubbler3-shot
      :bubbler-sharp
      :spur-shot
      :spur-trail
      :whimsical-star
      nil
      nil
      nil
      nil
      :shots-end
      :omega-body
      :omega-leg
      :omega-strut
      :omega-shot
      nil
      :ironh
      nil
      nil
      nil
      nil
      :x-mainobject
      :x-body
      :x-tread
      :x-internals
      :x-door
      :x-target
      :x-fishy-spawner
      nil
      nil
      nil
      :sisters-head
      :sisters-body
      :sisters-main
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      :udcore-main
      :udcore-front
      :udcore-back
      :udcore-face
      :udmini-rotator
      :udmini-bbox
      nil
      nil
      nil
      nil
      :heavy-press
      :heavy-press-shield
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      :ballos-main
      :ballos-body
      :ballos-eye
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil
      nil)
  "A table of TSC type index to entity type.")

;; Following is heavily sourced from
;; http://www.cavestory.org/guides/tsc_r2.txt
(defvar* *tsc-command-table*
    '(((:fn . :AE+) (:command . "AE+") (:description . "Refill ammo"))
      ((:fn . :AM+) (:command . "AM+") (:description . "Get weapon X, add Y to max ammo (just adds ammo if you have the weapon)"))
      ((:fn . :AM-) (:command . "AM-") (:description . "Lose weapon X"))
      ((:fn . :AMJ) (:command . "AMJ") (:description . "Jump to event Y if you have weapon X"))
      ((:fn . :ANP) (:command . "ANP") (:description . "Animate entity X with method Y in direction Z [entity type determines Y values?]"))
      ((:fn . :BOA) (:command . "BOA") (:description . "Animate boss entity. Give map-boss scriptstate X."))
      ((:fn . :BSL) (:command . "BSL") (:description . "Start boss fight with entity W. Use 0000 to end the boss fight. (NPC flag 0200 must be set; should work with anything that has HP)"))
      ((:fn . :CAT) (:command . "CAT") (:description . "Instantly display text. Use before a <MSG/2/3; works until <END. Same command as <SAT."))
      ((:fn . :CIL) (:command . "CIL") (:description . "Credits: Clear the illustration."))
      ((:fn . :CLO) (:command . "CLO") (:description . "Close the text box (used after MSG/MS2/MS3)"))
      ((:fn . :CLR) (:command . "CLR") (:description . "Clear the text box (used after MSG/MS2/MS3)"))
      ((:fn . :CMP) (:command . "CMP") (:description . "Change map coords X:Y to tile Z. Produces Smoke."))
      ((:fn . tsc-change-music) (:command . "CMU") (:description . "Change music to song X") (:arg-keys :song-num))
      ((:fn . :CNP) (:command . "CNP") (:description . "Change all entities X to entity type Y with direction Z"))
      ((:fn . :CPS) (:command . "CPS") (:description . "Stop propeller sound (used after SPS) (from helicopter cutscene after final battles)"))
      ((:fn . :CRE) (:command . "CRE") (:description . "Roll credits"))
      ((:fn . :CSS) (:command . "CSS") (:description . "Stop stream sound (used after SSS) (from River area)"))
      ((:fn . :DNA) (:command . "DNA") (:description . "Remove all entities of type X."))
      ((:fn . :DNP) (:command . "DNP") (:description . "Entity X is removed completely"))
      ((:fn . :ECJ) (:command . "ECJ") (:description . "Jump to event Y if any entity with ID X is present"))
      ((:fn . :END) (:command . "END") (:description . "End scripted event"))
      ((:fn . :EQ+) (:command . "EQ+") (:description . "Add X to equip flag bytes"))
      ((:fn . :EQ-) (:command . "EQ-") (:description . "Subtract X from equip flag bytes"))
      ((:fn . :ESC) (:command . "ESC") (:description . "Quit to title screen"))
      ((:fn . :EVE) (:command . "EVE") (:description . "Jump to event X (non-conditional)"))
      ((:fn . :FAC) (:command . "FAC") (:description . "Show face X in text box"))
      ((:fn . :FAI) (:command . "FAI") (:description . "Fade in with direction X") (:arg-keys :dir-num))
      ((:fn . :FAO) (:command . "FAO") (:description . "Fade out with direction X"))
      ((:fn . :FL+) (:command . "FL+") (:description . "Set flag X"))
      ((:fn . :FL-) (:command . "FL-") (:description . "Clear flag X"))
      ((:fn . :FLA) (:command . "FLA") (:description . "Flash the screen white"))
      ((:fn . :FLJ) (:command . "FLJ") (:description . "Jump to event Y if flag X is set"))
      ((:fn . :FMU) (:command . "FMU") (:description . "Fade the music to a low volume (good to use before CMU)"))
      ((:fn . :FOB) (:command . "FOB") (:description . "Focus on boss X in Y ticks. Use Y > 0."))
      ((:fn . :FOM) (:command . "FOM") (:description . "Focus view on you (normal view), view movement takes X ticks (WARNING: speed 0000 crashes)"))
      ((:fn . :FON) (:command . "FON") (:description . "Focus view on entity X, view movement takes Y ticks"))
      ((:fn . :FRE) (:command . "FRE") (:description . "Frees menu cursor [also used after ZAM for some reason?]"))
      ((:fn . :GIT) (:command . "GIT") (:description . "Show weapon/item X icon above text box - add 1000 to X for items - GIT0000 to hide"))
      ((:fn . :HMC) (:command . "HMC") (:description . "Removes main character entity (use SMC after)"))
      ((:fn . :INI) (:command . "INI") (:description . "Resets memory and starts game from the beginning"))
      ((:fn . :INP) (:command . "INP") (:description . "Change entity X to type Y with direction Z and set entity flag 100 (0x8000)."))
      ((:fn . :IT+) (:command . "IT+") (:description . "Get item X"))
      ((:fn . :IT-) (:command . "IT-") (:description . "Lose item X"))
      ((:fn . :ITJ) (:command . "ITJ") (:description . "Jump to event Y if you have item X"))
      ((:fn . :KEY) (:command . "KEY") (:description . "Hides status bars and locks out input to your character until END (used with MSG/MS2/MS3 and PRI)"))
      ((:fn . :LDP) (:command . "LDP") (:description . "Loads profile.dat into memory and starts game from save"))
      ((:fn . :LI+) (:command . "LI+") (:description . "Restore X amount of health"))
      ((:fn . :ML+) (:command . "ML+") (:description . "Max health increased X amount"))
      ((:fn . :MLP) (:command . "MLP") (:description . "Display map [how is this used without blanking screen while map is displayed?]"))
      ((:fn . :MM0) (:command . "MM0") (:description . "Instantly halts your horizontal motion"))
      ((:fn . :MNA) (:command . "MNA") (:description . "Displays name of current map"))
      ((:fn . :MNP) (:command . "MNP") (:description . "Move entity X to coords Y:Z facing direction W"))
      ((:fn . :MOV) (:command . "MOV") (:description . "Move you to coords X:Y"))
      ((:fn . :MP+) (:command . "MP+") (:description . "Set a map flag."))
      ((:fn . :MPJ) (:command . "MPJ") (:description . "Jump to event X if map flag is set for the current area."))
      ((:fn . :MS2) (:command . "MS2") (:description . "Open invisible text box at top of screen (text follows)"))
      ((:fn . :MS3) (:command . "MS3") (:description . "Open normal text box at top of screen (text follows)"))
      ((:fn . :MSG) (:command . "MSG") (:description . "Open normal text box (text follows)"))
      ((:fn . :MYB) (:command . "MYB") (:description . "Knocks you back from direction X (0000 knocked right, 0002 knocked left, any other just hops in place)"))
      ((:fn . :MYD) (:command . "MYD") (:description . "Make you face direction X"))
      ((:fn . :NCJ) (:command . "NCJ") (:description . "Jump to event Y if any entity of type X is present"))
      ((:fn . :NOD) (:command . "NOD") (:description . "Text box wait for button press (used after MSG/MS2/MS3)"))
      ((:fn . :NUM) (:command . "NUM") (:description . "Prints the value [4a5b34+W*4] to the message box. Use 0000 to print the last used X from compatible commands (eg AM+)."))
      ((:fn . :PRI) (:command . "PRI") (:description . "Hides status bars and freezes game action until KEY or END (used with MSG/MS2/MS3)"))
      ((:fn . :PS+) (:command . "PS+") (:description . "Set teleporter slot X to location Y"))
      ((:fn . :QUA) (:command . "QUA") (:description . "Shake the screen for X ticks"))
      ((:fn . :RMU) (:command . "RMU") (:description . "Restore music playback"))
      ((:fn . :SAT) (:command . "SAT") (:description . "Instant text display on all messages until END (glitches scrolling text)"))
      ((:fn . :SIL) (:command . "SIL") (:description . "Show illustration during credits (use CIL after)"))
      ((:fn . :SK+) (:command . "SK+") (:description . "Set skipflag X (remains set until program exits, to avoid repeating cutscenes/dialogue after retrying)"))
      ((:fn . :SK-) (:command . "SK-") (:description . "Clear skipflag X"))
      ((:fn . :SKJ) (:command . "SKJ") (:description . "Jump to event Y if skipflag X is set"))
      ((:fn . :SLP) (:command . "SLP") (:description . "Teleporter location menu"))
      ((:fn . :SMC) (:command . "SMC") (:description . "Restores main character entity (used after HMC)"))
      ((:fn . :SMP) (:command . "SMP") (:description . "Jump to event X if skipflag W is set. Does not create smoke."))
      ((:fn . :SNP) (:command . "SNP") (:description . "Create an entity of type X at coordinates Y:Z with direction W."))
      ((:fn . tsc-sound-effect) (:command . "SOU") (:description . "Play sound effect X") (:arg-keys :sound-effect-num))
      ((:fn . :SPS) (:command . "SPS") (:description . "Start propeller sound (use CPS after) (from helicopter cutscene after final battles)"))
      ((:fn . :SSS) (:command . "SSS") (:description . "Start stream sound at pitch X (use CSS after) (from River area - normal pitch is 0400)"))
      ((:fn . :STC) (:command . "STC") (:description . "Saves the current time to 290.rec"))
      ((:fn . :SVP) (:command . "SVP") (:description . "Save game"))
      ((:fn . :TAM) (:command . "TAM") (:description . "Trade weapon X for weapon Y, set max ammo to Z (max ammo 0000 = no change) (GLITCH: first weapon 0000)"))
      ((:fn . tsc-transport) (:command . "TRA") (:description . "Load map X, run event Y, transport you to coords Z:W") (:arg-keys :map-num :script-id :tile-x :tile-y))
      ((:fn . :TUR) (:command . "TUR") (:description . "Instantly display text. Use after a <MSG/2/3; works until another <MSG/2/3 or an <END."))
      ((:fn . :UNI) (:command . "UNI") (:description . "0000 normal / 0001 zero-g movement, facing direction is locked (disables focus commands) (from Stream boss) / 0002 movement is locked, can still fire"))
      ((:fn . :WAI) (:command . "WAI") (:description . "Pause script for X ticks"))
      ((:fn . :WAS) (:command . "WAS") (:description . "Pause script until your character touches the ground"))
      ((:fn . :XX1) (:command . "XX1") (:description . "Show the island falling in manner W. Use 0000 to have it crash and 0001 to have it stop midway."))
      ((:fn . :YNJ) (:command . "YNJ") (:description . "Ask yes or no, jump to event X if No"))
      ((:fn . :ZAM) (:command . "ZAM") (:description . "All weapons drop to level 1")))
  "List of (COMMAND DESCRIPTION ARG-KEYS) for tsc script <XYZ commands.
The arg-keys give a description of the arguments (rather than just positional).
They are used by the implementations of the functions.")

(defvar* *directions-table*
    #(:left :up :right :down :center)
  "For TSC directions.
NOTE: For MYB it is 0000 Right, 0002 Left (reversed).")

(defvar* *maps-table*
    #((:0 "Credits")
      (:Pens1 "Arthur's House - normal")
      (:Eggs "Egg Corridor")
      (:EggX "Egg No. 00 - normal")
      (:Egg6 "Egg No. 06")
      (:EggR "Egg Observation Room")
      (:Weed "Grasstown")
      (:Santa "Santa's House")
      (:Chako "Chaco's House")
      (:MazeI "Labyrinth I (vertical starting room)")
      (:Sand "Sand Zone - normal")
      (:Mimi "Mimiga Village")
      (:Cave "First Cave")
      (:Start "Start Point")
      (:Barr "Shack (Mimiga Village)")
      (:Pool "Reservoir")
      (:Cemet "Graveyard")
      (:Plant "Yamashita Farm")
      (:Shelt "Shelter (Grasstown)")
      (:Comu "Assembly Hall (Mimiga Village)")
      (:MiBox "Save Point (Mimiga Village)")
      (:EgEnd1 "Side Room (Egg Corridor)")
      (:Cthu "Cthulhu's Abode (Egg Corridor)")
      (:Egg1 "Egg No. 01")
      (:Pens2 "Arthur's House - Sue on computer")
      (:Malco "Power Room (Grasstown)")
      (:WeedS "Save Point (Grasstown)")
      (:WeedD "Execution Chamber (Grasstown)")
      (:Frog "Gum (Grasstown)")
      (:Curly "Sand Zone Residence")
      (:WeedB "Grasstown Hut")
      (:Stream "Main Artery (Waterway)")
      (:CurlyS "Small Room (Sand Zone)")
      (:Jenka1 "Jenka's House - normal")
      (:Dark "Deserted House (Sand Zone)")
      (:Gard "Sand Zone Storehouse")
      (:Jenka2 "Jenka's House - after Balrog attacks")
      (:SandE "Sand Zone - after boss fight")
      (:MazeH "Labyrinth H (sliding block room)")
      (:MazeW "Labyrinth W (main area w/shop, camp)")
      (:MazeO "Camp (Labyrinth)")
      (:MazeD "Clinic Ruins (Labyrinth)")
      (:MazeA "Labyrinth Shop")
      (:MazeB "Labyrinth B (booster)")
      (:MazeS "Boulder Chamber (Labyrinth)")
      (:MazeM "Labyrinth M (gaudi eggs)")
      (:Drain "Dark Place (Labyrinth)")
      (:Almond "Core (Labyrinth)")
      (:River "Waterway")
      (:Eggs2 "Egg Corridor?")
      (:Cthu2 "Cthulhu's Abode? (Egg Corridor?)")
      (:EggR2 "Egg Observation Room?")
      (:EggX2 "Egg No. 00 - hatched")
      (:Oside "Outer Wall")
      (:EgEnd2 "Side Room (Egg Corridor?)")
      (:Itoh "Storehouse (Outer Wall)")
      (:Cent "Plantation")
      (:Jail1 "Jail No. 1 (Plantation)")
      (:Momo "Hideout (Plantation)")
      (:Lounge "Rest Area (Plantation)")
      (:CentW "Teleporter (Plantation)")
      (:Jail2 "Jail No. 2 (Plantation)")
      (:Blcny1 "Balcony - normal")
      (:Priso1 "Last Cave")
      (:Ring1 "Throne Room (Balcony)")
      (:Ring2 "The King's Table (Balcony)")
      (:Prefa1 "Prefab House (Balcony) - normal")
      (:Priso2 "Last Cave Hidden")
      (:Ring3 "Black Space (Balcony)")
      (:Little "Little House (Outer Wall)")
      (:Blcny2 "Balcony - after boss fights")
      (:Fall "Ending")
      (:Kings "Intro")
      (:Pixel "Waterway Cabin")
      (:e_Maze "Credits - Labyrinth")
      (:e_Jenk "Credits - Jenka's House")
      (:e_Malc "Credits - Power Room")
      (:e_Ceme "Credits - Graveyard")
      (:e_Sky "Credits - Sky")
      (:Prefa2 "Prefab House (Balcony) - entrance to hell")
      (:Hell1 "Sacred Ground B1")
      (:Hell2 "Sacred Ground B2")
      (:Hell3 "Sacred Ground B3")
      (:Mapi "Storage (Graveyard)")
      (:Hell4 "Passage? - normal")
      (:Hell42 "Passage? - from Sacred Ground B3")
      (:Statue "Statue Chamber (Plantation/Sacred Grounds)")
      (:Ballo1 "Seal Chamber (Sacred Grounds) - normal")
      (:Ostep "Corridor (Sacred Grounds)")
      (:e_Labo "Credits - Laboratory")
      (:Pole "Hermit Gunsmith")
      (:Island "[map is totally blank - TSC is called right before good/best endings]")
      (:Ballo2 "Seal Chamber (Sacred Grounds) - after boss fight")
      (:e_Blcn "Credits - Balcony")
      (:Clock "Clock Room (Outer Wall)"))
  "Ordered list of (map-key description) for TSC map ids.")

(defun map-idx->map-keyword (idx)
  "Returns a keyword representing the map for the given tsc idx."
  (first (elt *maps-table* idx)))

(defvar* *weapons-table*
    #(nil
      :Snake
      :Polar-Star
      :Fireball
      :Machine-Gun
      :Missile-Launcher
      :Missiles
      :Bubbler
      nil
      :Blade
      :Super-Missile-Launcher
      :Super-Missiles
      :Nemesis
      :Spur)
  "Ordered list of weapon-keys for TSC weapon ids.")

(defun weapon-idx->weapon-keyword (idx)
  "Return the keyword for a weapon given its tsc idx."
  (elt *weapons-table* idx))

(defvar* *item-table*
    #(:blank
      :arthurs-key
      :Map-System
      :Santas-Key
      :Silver-Locket
      :Beast-Fang
      :Life-Capsule
      :ID-Card
      :Jellyfish-Juice
      :Rusty-Key
      :Gum-Key
      :Gum-Base
      :Charcoal
      :Explosive
      :Puppy
      :Life-Pot
      :Cure-All
      :Clinic-Key
      :Booster-0.8
      :Arms-Barrier
      :Turbocharge
      :Curlys-Air-Tank
      :Nikumaru-Counter
      :Booster-v2.0
      :Mimiga-Mask
      :Teleporter-Room-Key
      :Sues-Letter
      :Controller
      :Broken-Sprinkler
      :Sprinkler
      :Tow-Rope
      :Clay-Figure-Medal
      :Little-Man
      :Mushroom-Badge
      :Ma-Pignon
      :Curlys-Underwear
      :Alien-Medal
      :Chacos-Lipstick
      :Whimsical-Star
      :Iron-Bond)
  "Ordered list of items indexed by TSC item id.")

(defun item-idx->keyword (idx)
  "TSC item idx -> keyword."
  (elt *item-table* idx))

(defvar* *equip-flags*
    '((:Booster-v0.8 0001)
      (:Map-System 0002)
      (:Arms-Barrier 0004)
      (:Turbocharge 0008)
      (:Air-Tank 0016)
      (:Booster-v2.0 0032)
      (:Mimiga-Mask 0064)
      (:Whimsical-Star 0128)
      (:Nikumaru-Counter 0256))
  "Flags TSC uses to set equipped items.")

(defun equip-flags-num->equip-flags (num)
  "Takes an integer flags and parses it into a set of *equip-flags*."
  (mapcar #'car
	  (remove-if (lambda (f) (zerop (logand num (second f))))
		     *equip-flags*)))

(defvar* *face-table*
    #(:blank
      :Sue-smile
      :Sue-frown
      :Sue-angry
      :Sue-hurt
      :Balrog-normal
      :Toroko-normal
      :King
      :Toroko-angry
      :Jack
      :Kazuma
      :Toroko-rage
      :Igor
      :Jenka
      :Balrog-smile
      :Misery-normal
      :Misery-smile
      :Booster-hurt
      :Booster-normal
      :Curly-smile
      :Curly-frown
      :Doctor
      :Momorin
      :Balrog-hurt
      :Broken-robot
      :Curly
      :Misery-angry
      :Human-Sue
      :Itoh
      :Ballos)
  "Ordered list of TSC ids for showing character faces in dialogue.")

(defun face-idx->keyword (idx)
  "TSC face idx (for messages) -> keyword."
  (elt *face-table* idx))

(defvar* *illustrations-table*
    #(:riding-Sky-Dragon
      :fighting-Core
      :fighting-Misery
      :Momorins-rocket
      :Outer-Wall
      :fighting-Ironhead
      :fighting-Balrog
      :Clinic
      :King-fighting-the-Doctor
      :Jenka-with-puppies
      :Curly-with-Mimiga-children
      :riding-Balrog
      nil
      :Hell
      :floating-island-blue-sky
      :floating-island-orange-sky
      :King-Jack-Sue
      :Ballos)
  "Ordered list of TSC ids for showing illustrations (in credits).
NOTE: any other values (including 0013) show :riding-sky-dragon")

(defun illustrations-idx->keyword (idx)
  "TSC face idx (for credits) -> keyword."
  (elt *illustrations-table* idx))

(defvar* *npc-data* (read-npc-table "./content/npc.tbl")
  "Alist read in from the npc table.")
(defvar* *smoke-amounts-table* #(0 3 7 12)
  "Table for use by the npc.tbl file. 
The number of smoke particles to create when destroyed.")

(defun entity-type-idx (entity-type)
  "Returns the index of entity-type in the entity-type table."
  (position entity-type *entity-type-table*))

(defun entity-npc-data (entity-type)
  "Returns the npc-data for a given entity-type keyword."
  (let ((e (mapcar (lambda (pair)
		     (cons (car pair)
			   (elt (cdr pair) (entity-type-idx entity-type))))
		   *npc-data*))
	(get-sound (comp first sound-effect-idx->sound-effect-key)))
    (aupdate e
	     :smoke-amt #_(elt *smoke-amounts-table* _)
	     :hurt-sound get-sound
	     :death-sound get-sound
	     :default-flags #'pxe-flags->entity-flags)))

(defun pickup-fns-alist ()
  (alist :pickup-rect-fn (lambda (a) (centered-rect (aval a :pos)
						    (both-v (/ *tile-size* 2))))
	 :pickup-kill-fn #'pickup-kill
	 :ai-fn #'pickup-ai
	 :draw-fn #'pickup-drawing))

(defun make-heart-or-missile-pickup (pos type bundle?)
  "Type is :heart or :missile."
  (amerge
   (pickup-fns-alist)
   (alist :subsystems *pickup-subsystems*)
   (make-pickup :type type :amt (ecase type
				  (:heart (if bundle? 6 2))
				  (:missile (if bundle? 3 1))))
   (alist :timers
	  '(:life-timer :anim-cycle)
	  :id (gen-entity-id)
	  :life-timer
	  (make-expiring-timer (s->ms 8) t)
	  :pos pos
	  :anim-cycle
	  (make-fps-cycle 14 (iota 2))
	  :type type
	  :bundle? bundle?)))

(defun make-heart-pickup (pos bundle?)
  (make-heart-or-missile-pickup pos :heart bundle?))

(defun make-missile-pickup (pos bundle?)
  (make-heart-or-missile-pickup pos :missile bundle?))

(defun pickup-drawing (a)
  (let* ((tm (aval a :life-timer))
	 (bundle? (aval a :bundle?))
	 (src-pos (ecase (aval a :type)
		    (:heart (if bundle?
				(tile-v 4 5)
				(tile-v 2 5)))
		    (:missile
		     (if bundle?
			 (tile-v 0 7)
			 (tile-v 0 5))))))
    (list
     (if (> (aval tm :ms-remaining) (* 2 *frame-time*))
	 (unless (death-flash? tm)
	   (make-sprite-drawing
	    :layer :pickup
	    :sheet-key :npc-sym
	    :src-rect
	    (tile-rect (+ (anim-cycle-offset a) src-pos))
	    :pos (- (aval a :pos) (tile-dims/2))))
	 (make-sprite-drawing
	  :layer :pickup
	  :sheet-key :npc-sym
	  :src-rect (tile-rect (tile-v 1 0))
	  :pos (- (aval a :pos) (tile-dims/2)))))))

(defvar* *door-enemy-subsystems*
    '(:physics :timers :drawable :damageable :damage-collision))

(defun door-enemy-fns-alist ()
  (let ((rect-fn
	 (lambda (d)
	   (tile-rect (tile-pos (aval d :tile-pos))))))
    (alist :ai-fn #'door-enemy-ai
	   :draw-fn #'door-enemy-drawings
	   :origin-fn
	   (lambda (d)
	     (tile-pos (+ (make-v 1/2 1/2) (aval d :tile-pos))))
	   :damageable-hit-react-fn #'door-enemy-hit-react
	   :damageable-rect-fn rect-fn
	   :damage-collision-rect-fn rect-fn
	   :damage-collision-amt-fn (constantly 1))))

(defun make-door-enemy (tile-pos)
  (amerge
   (door-enemy-fns-alist)
   (alist :subsystems *door-enemy-subsystems*)
   (alist
    :tile-pos tile-pos
    :eye-state :closed
    :id (gen-entity-id))))

(defun make-door-drawing (pos)
  (make-sprite-drawing
   :layer :npc
   :sheet-key :npc-sym
   :src-rect (create-rect (tile-v 14 1)
			  (tile-v 1 3/2))
   :pos pos))

(setfn door-enemy-ai
       shake-ai
       door-eye-ai)

(defun door-enemy-drawings (d)
  (let ((pos
	 (+ (physics-pos d)
	    (tile-pos (- (aval d :tile-pos) (make-v 0 1/2))))))
    (list
     (cond
       ((member :shake-timer (aval d :timers))
	(make-sprite-drawing
	 :layer :npc
	 :sheet-key :npc-sym
	 :src-rect (create-rect (tile-v 15 5)
				(tile-v 1 3/2))
	 :pos pos))
       ((or (door-eye-closing? d) (door-eye-opening? d))
	(make-sprite-drawing
	 :layer :npc
	 :sheet-key :npc-sym
	 :src-rect (create-rect (tile-v 13 5)
				(tile-v 1 3/2))
	 :pos pos))
       ((door-eye-open? d)
	(make-sprite-drawing
	 :layer :npc
	 :sheet-key :npc-sym
	 :src-rect (create-rect (tile-v 14 5)
				(tile-v 1 3/2))
	 :pos pos))
       (t (make-door-drawing pos))))))

(defun door-eye-opening? (d)
  (eq :opening (aval d :eye-state)))

(defun door-eye-open? (d)
  (eq :open (aval d :eye-state)))

(defun door-eye-closing? (d)
  (eq :closing (aval d :eye-state)))

(defun door-eye-ai (d)
  (let ((player-state (estate (entity-id :player))))
    (setq d (aset d :player-origin (if player-state
				       (origin player-state)
				       (aval d :player-origin)))))
  (let* ((player-x (x (aval d :player-origin)))
	 (door-x (x (origin d)))
	 (player-in-range?
	  (< (abs (- player-x door-x)) (* 4 *tile-size*))))
    (cond ((door-eye-open? d)
	   (if (not player-in-range?)
	       (aupdate d
			:eye-state (constantly :closing)
			:eye-timer (constantly (make-expiring-timer 5 :t))
			:timers (pushfn :eye-timer))
	       d))
	  ((door-eye-opening? d)
	   (if (ticked? d :eye-timer)
	       (aset d :eye-state :open)
	       d))
	  ((door-eye-closing? d)
	   (if (ticked? d :eye-timer)
	       (aset d :eye-state :closed)
	       d))
	  (t
	   (if player-in-range?
	       (aupdate d
			:eye-state (constantly :opening)
			:eye-timer (constantly (make-expiring-timer 5 :t))
			:timers (pushfn :eye-timer))
	       d)))))

(setfn door-enemy-hit-react
       damage-reaction
       shake-hit-react)

(defvar* *spike-subsystems*
    '(:drawable :damage-collision))

(defun spike-fns-alist ()
  (alist :draw-fn #'spike-drawings
	 :damage-collision-rect-fn #'spike-rect
	 :damage-collision-amt-fn (constantly 5)))

(defun spike-rect (s)
  (let* ((pos (tile-pos->pos (aval s :tile-pos)))
	 (skinny 1/3)
	 (tall 3/4)
	 (skinny/2 (/ skinny 2)))
    (ecase (aval s :dir)
      (:left (make-rect :pos (+ pos (tile-v (- 1 tall) (- 1/2 skinny/2)))
			:size (tile-v tall skinny)))
      (:right (make-rect :pos (+ pos (tile-v 0 (- 1/2 skinny/2)))
			 :size (tile-v tall skinny)))
      (:up (make-rect :pos (+ pos (tile-v (- 1/2 skinny/2) (- 1 tall)))
		      :size (tile-v skinny tall)))
      (:down (make-rect :pos (+ pos (tile-v (- 1/2 skinny/2) 0))
			:size (tile-v skinny tall))))))

(defun make-spike (tile-pos dir)
  (amerge
   (spike-fns-alist)
   (alist :subsystems *spike-subsystems*)
   (alist
    :tile-pos tile-pos
    :dir dir
    :id (gen-entity-id))))

(defun spike-drawings (s)
  (list
   (make-sprite-drawing
    :layer :npc
    :sheet-key :npc-sym
    :src-rect (tile-rect (tile-v (+ 16
				    (ecase (aval s :dir)
				      (:left 0)
				      (:up 1)
				      (:right 2)
				      (:down 3)))
				 25/2))
    :pos (tile-pos (aval s :tile-pos)))))
