;;;; cave-story.lisp

(in-package #:cave-story)

(defvar *window*)
(defvar *renderer*)
(defvar *font*)
(defvar *global-game*)

(defparameter *stage-viewer* nil)
(defvar *stage-viewer-camera-pos* (scale-v *window-dims* 1/2))

;; Debug params.
(defparameter *update-period* 1
  "Number of frames per update. 1 for real-time.")

(defparameter *debug-input-keybindings*
  `((((:key :escape)) .
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
		      (update! *global-game*)))))))

(defun make-game
    (&key player camera stage projectile-groups hud gun-exps
       damage-numbers active-systems (input (make-input)))
  (alist :player player
	 :camera camera
	 :stage stage
	 :projectile-groups projectile-groups
	 :gun-exps gun-exps
	 :hud hud
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

      (setq *stage-viewer-camera-pos*
	    (+v *stage-viewer-camera-pos*
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
      (camera-pos (estate (aval *global-game* :camera))
		  (stage-dims->camera-bounds
		   (stage-dims (aval *global-game* :stage))))))

(defvar *update-rolling-average* (make-rolling-average (* *fps* 3)))
(defvar *frame-rolling-average* (make-rolling-average (* *fps* 3)))
(defvar *render-rolling-average* (make-rolling-average (* *fps* 3)))
(defvar *last-update-time*)
(defvar *frame-timer*)

(defun update-and-render! ()
  (if *global-paused?*
      (draw-text-line! (zero-v) "PAUSED")
      (progn
	(rolling-average-time *update-rolling-average*
	  (setq *global-game* (update! *global-game*)))

	(draw-text-line!
	 (make-v 0 (- (y *window-dims*) *tile-size*))
	 (format nil "Renderer: ~,0f%"
		 (rolling-average-percent *render-rolling-average*)))
	(draw-text-line!
	 (make-v (* 6 *tile-size*) (- (y *window-dims*) *tile-size*))
	 (format nil "Update: ~,0f%"
		 (rolling-average-percent *update-rolling-average*)))
	(draw-text-line!
	 (make-v (* 12 *tile-size*) (- (y *window-dims*) *tile-size*))
	 (format nil "Total: ~,0f%"
		 (rolling-average-percent *frame-rolling-average*)))

	(rolling-average-time *render-rolling-average*
	  (render! *render-list* (current-camera-pos)))))

  
  (setq *frame-timer* (- *frame-timer*
			 (* *update-period* *frame-time*))))

(defun main-loop-iteration! ()
  (let ((transient-input (gather-transient-input!)))
    (handle-debug-input! transient-input)
    (setq *global-game*
	  (aset *global-game*
		:input (gather-input (aval *global-game* :input)
				     transient-input))))
  (when (>= *frame-timer* (* *update-period* *frame-time*))
    (rolling-average-time *frame-rolling-average* (update-and-render!)))

  (let ((dt (- (sdl:get-ticks) *last-update-time*)))
    ;; NOTE: if we are paused beyond our control, Don't play catchup.
    (setq *frame-timer*
	  (+ *frame-timer*
	     (min dt (* 2 *frame-time*)))))
  (setq *last-update-time* (sdl:get-ticks))
  (music-update!)
  (sdl:delay 1))

(defun main! ()
  "Entry point to the game."
  (catch 'exit
    (unwind-protect
	 (progn
	   (setq *frame-timer* 0)
	   (setq *global-game* (init!))
	   (setq *update-rolling-average* (make-rolling-average (* *fps* 3)))
	   (setq *render-rolling-average* (make-rolling-average (* *fps* 3)))
	   (setq *frame-rolling-average* (make-rolling-average (* *fps* 3)))
	   (setq *last-update-time* (sdl:get-ticks))
	   (loop do
		(swank-tools:update)
		(swank-tools:continuable
		  (main-loop-iteration!))))
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
  (alist :ai-fn #'pickup-ai
	 :draw-fn #'dorito-drawing
	 :stage-collision-fn #'dorito-stage-collision
	 :pickup-rect-fn #'dorito-pickup-rect))


(defparameter *pickup-subsystems*
  '(:timers :drawable :pickup))

(defun make-dorito (pos size)
  (let ((vel (polar-vec->v (rand-angle) 0.07)))
    (amerge
     (dorito-fns-alist)
     (alist :subsystems (list* :physics :stage-collision *pickup-subsystems*))
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
       (+v (anim-cycle-offset d)
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

(setfn pickup-kill
       (comp 
	(aupdatefn :sound-effects (pushfn :pickup))
	(asetfn :dead? t)))

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
    (unless (aval sp :dead?)
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
(defun make-hud ()
  (amerge
   (hud-fns-alist)
   (alist :subsystems *hud-subsystems*)
   (alist :id (gen-entity-id)
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

    (let ((health (aval (estate (aval *global-game* :player)) :health-amt)))
      (when (timer-active? (aval hud :health-change-timer))
	(push
	 (make-sprite-drawing
	  :layer :hud
	  :sheet-key :text-box
	  :src-rect
	  (create-rect-cmpts 0 (tiles/2 4)
			     (floor (* (tiles/2 bar-tile/2-w)
				       (/ (aval hud :last-health-amt)
					  (aval (estate
						 (aval *global-game* :player))
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
					(aval (estate
					       (aval *global-game* :player))
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

      (multiple-value-bind (exp gun-name) (current-gun-exp (aval *global-game*
								 :player)
							   (aval *global-game*
								 :gun-exps))
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
		(aval (estate (aval *global-game* :player)) :health-amt)))))

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
  (pos->tile-pos (+v mouse-pos camera-pos)))

(defun update! (game)
  "The Main Loop, called once per *FRAME-TIME*."
  (declare (optimize debug))
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

  (unless *stage-viewer*
    (let ((active-update-systems
	   (aval (estate (aval game :active-systems)) :update))
	  (stage (aval game :stage))
	  (player (aval game :player)))
      (update-timers-subsystem! active-update-systems)
      (update-physics-subsystem! active-update-systems)
      (update-bullet-subsystem! active-update-systems)
      (update-stage-collision-subsystem! active-update-systems stage)
      (update-pickup-subsystem! active-update-systems player)

      (update-damage-collision-subsystem! active-update-systems player)
      (update-dynamic-collision-subsystem! active-update-systems player)))

  (let ((active-draw-systems (aval (estate (aval game :active-systems)) :draw)))
    (update-drawable-subsystem! active-draw-systems))

  (remove-all-dead! game)

  ;; Debug Drawings Below.

  ;; (draw-point! (player-nozzle-pos player) *red*)
  (let ((focus (camera-focus (estate (aval game :camera))))
	(camera-bounds (stage-dims->camera-bounds
			(stage-dims (aval game :stage)))))
    (unless *stage-viewer*
      (draw-point! focus *cyan*)
      (draw-point! (clamp-pos focus camera-bounds) *red*))
    (draw-rect! camera-bounds *cyan* :layer :debug-camera))
  (unless *stage-viewer*
    (draw-point! (camera-target-from-player (estate (aval game :player)))
		 *white*))
  (let ((mouse-pos (input-mouse-coords (aval game :input))))
    (draw-point! mouse-pos
		 *white*
		 :layer :mouse)
    (let ((tp (mouse->tile-pos mouse-pos (current-camera-pos))))
      (draw-text-line! (make-v 320 0)
		       (format nil "TILE: [~A, ~A]"
			       (x tp) (y tp)))))

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
  (let* ((gun-name (player-current-gun-name (estate player)))
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

(defun cave-stage ()
  (stage-from-file-data
   (read-pxm-file "./content/stages/Cave.pxm")
   (read-pxa-file "./content/stages/Cave.pxa")))

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
		(make-critter (tile-pos (aval entity :tile-pos)))))))
	(when e
	  (amerge data e))))))

(defun make-pxe-entities (pxe flags)
  "Convert a parsed pxe list to a list of entities."
  (remove nil (mapcar #_(make-pxe-entity _ flags) pxe)))

(defun create-game! ()
  (let ((damage-numbers (make-damage-numbers))
	(projectile-groups (make-projectile-groups))
	(stage (make-stage (cave-stage)))
	(entities (make-pxe-entities
		   (read-pxe-file "./content/stages/Cave.pxe")
		   nil))
	(hud (make-hud))
	(player (make-player :pos (tile-v 48 37)))
	(gun-exps (make-gun-exps))
	(active-systems (make-active-systems)))
    
    (let ((camera (make-camera (physics-pos player) (zero-v) player)))
      (mapc #'create-entity!
	    (list*
	     damage-numbers
	     projectile-groups
	     stage
	     hud
	     player
	     gun-exps
	     active-systems
	     camera

	     entities
	     ;; (make-critter (tile-v (+ 14 1/4) 6))
	     ;; (make-elephant (tile-v 7 6))
	     ;; (make-dorito (tile-v (+ 14 1/4) 6) :medium))
	     ))

      (make-game :player (aval player :id)
		 :camera (aval camera :id)
		 :stage stage
		 :hud (aval hud :id)
		 :projectile-groups (aval projectile-groups :id)
		 :gun-exps (aval gun-exps :id)
		 :active-systems (aval active-systems :id)
		 :damage-numbers (aval damage-numbers :id)))))

(defun reset! ()
  ;;(switch-to-new-song! :lastcave)
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
	'()
	'(:target-texture)))
  (sdl:set-render-draw-blend-mode *renderer* :blend)
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

(defparameter *bat-subsystems*
  '(:timers :damage-collision :damageable :drawable :physics))

(defun make-bat (tile-pos)
  (amerge
   (bat-fns-alist)
   (alist :subsystems *bat-subsystems*)
   (alist
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
			     (tile-rect (+v (tile-v 2 2)
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
  (collect #_(make-death-cloud-particle pos) num))

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

(defun make-critter (pos)
  (let ((id (gen-entity-id)))
    (amerge
     (critter-fns-alist)
     (alist :subsystems *critter-subsystems*)
     (alist
      :stage-physics (gravity-kin-2d :pos pos)
      :physics '(:stage-physics)
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
  (aset obj :facing (face-player (physics-pos obj)
				 (aval *global-game* :player))))

(defun critter-jump-ai (c)
  (let ((facing (aval c :facing)))

    (if (and (not (timer-active? (aval c :sleep-timer)))
	     (< (origin-dist c (estate (aval *global-game* :player))) (tiles 4))
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
			  ((< (origin-dist c (estate (aval *global-game* :player)))
			      (tiles 7))
			   1)
			  (t
			   0))))
    (list
     (make-sprite-drawing :layer :enemy
			  :sheet-key :npc-cemet
			  :src-rect (tile-rect (+v (tile-v sprite-tile-x 0)
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

(defparameter *elephant-dims* (make-v (tiles 2) (tiles 3/2)))
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
      (create-rect (+v src-pos
		       (make-v 0 (if (eq (aval e :facing) :left)
				     0
				     (tiles 3/2))))
		   *elephant-dims*)
      :pos (physics-pos e)))))

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
    (let* ((d (estate (aval *global-game* :damage-numbers)))
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

(defun make-drops (origin amt)
  "Makes the equivalent number of pickups to amt from origin."
  ;; 3/5 of the time choose doritos, otherwise missile or heart.
  (let ((large-drop? (> amt 6)))
   (case (random 4)
     (0 ;;Missile
      )
     (1 ;;Heart
      )
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
	   :sound-effects (pushfn :enemy-hurt)))
	 (let ((origin (origin obj)))
	   (comp
	    (asetfn
	     :dead? t)
	    (aupdatefn
	     :new-entities
	     (appendfn
	      (make-drops origin (aval obj :exp-for-kill))
	      (make-num-death-cloud-particles (aval obj :smoke-amt)
					      origin))
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
	       (pushfn (timed-camera-shake (estate (aval *global-game* :camera))
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
       #'damage-reaction)
(setfn bat-ai #'face-player-ai)
(setfn critter-ai
       (comp face-player-ai
	     critter-jump-ai
	     shake-ai))
(setfn critter-hit-react
       (comp damage-reaction shake-hit-react))
(setfn elephant-hit-react 
       (comp damage-reaction
	     elephant-rage-hit-react
	     shake-hit-react))
(setfn elephant-ai
       (comp elephant-stage-physics-ai
	     elephant-rage-effects
	     elephant-rage-ai
	     shake-ai))

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

(defun pxm-and-attrs->stage (pxm attrs)
  "Create a stage from a list of row-major tiles, and a vector of
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

(defun stage-from-file-data (pxm pxa)
  "Create a stage given the file data."
  (pxm-and-attrs->stage pxm
			(map 'vector #'tile-attribute-num->tile-attributes
			     pxa)))

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
  (mapcar
   (lambda (d)
     (list (car d) (cons :script (cdr d))))
   (split-sequence:split-sequence-if
    (lambda (a) (and (consp a) (eq (car a) :end)))
    (mapcan #'parse-tsc-line
	    (remove-if (lambda (d) (zerop (length d)))
		       (split-sequence:split-sequence #\newline script)))
    :remove-empty-subseqs t)))

(defun rest-of-line (line start)
  (when (>= (length line) start)
    (subseq line start)))

(defun parse-tsc-line (line)
  (if (zerop (length line))
      nil
      (case (aref line 0)
	(#\# (list (cons :id (car (parse-tsc-num (subseq line 1))))))
	(#\< (let ((parse (parse-tsc-command line))) 
	       (cons (car parse) (parse-tsc-line (cdr parse)))))
	(t (let ((parse (parse-tsc-text line)))
	     (cons (car parse) (parse-tsc-line (cdr parse))))))))

(defun parse-tsc-text (line)
  "Assumes line starts with text for a message.
Parses up to the next <."
  (let ((text (first (split-sequence:split-sequence #\< line))))
    (cons text (rest-of-line line (length text)))))

(defun parse-tsc-command (line)
  "Assumes line starts with a tsc command <XYX0001:1234...
Returns (command . remaining-line)"
  (let* ((cmd (make-keyword (subseq line 1 4)))
	 (desc (second (assoc cmd *tsc-command-table*))))
    (if (and (> (length line) 4) (digit-char-p (aref line 4)))
	(let ((parse (parse-tsc-command-args (subseq line 4))))
	  (cons (list cmd
		      (cons :description desc)
		      (cons :args (car parse)))
		(cdr parse)))
	(cons (list cmd (cons :description desc))
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

(defparameter *entity-flags*
  '((:solid-mushy 		#x0001 "Pushes player out, but is not solid. Normal State for enemies.")
    (:ignore-tile		#x0002)
    (:invulnerable		#x0004 "Invulnerable. Plays clinking sound when shot.")
    (:ignore-solid		#x0008)
    (:bouncy			#x0010 "When :solid-brick is set, acts like a mini-trampoline")
    (:shootable			#x0020)
    (:solid-brick		#x0040 "Bounding box is solid, just like a solid tile.")
    (:no-rear-top-attack	#x0080 "When attacked from the rear or top, damage is 0.")
    (:script-on-touch		#x0100 "Activate tsc-id script when the player touches this entity.")
    (:script-on-death		#x0200 "Activate tsc-id script when the entity dies.")
    (:drop-powerups-dont-use	#x0400 "unused.")
    (:appear-on-flag-id		#x0800 "This entity should spawn if the flag-id is set")
    (:faces-right		#x1000 "Sets the initial direction the enemy is facing to be to the right.")
    (:script-on-activate	#x2000 "Activate tsc-id script when the player interacts with this entity.")
    (:disappear-on-flag-id	#x4000 "This entity should NOT spawn if the flag-id is set.")
    (:show-float-text		#x8000 "This enemy should have a floating-text associated with it."))
  "Flags that represent the entity's attributes. Set by npc.tbl and .tsc files.")

(defparameter *tile-attribute-flags*
  '((:solid-player	#x001 "Solid to the player.")
    (:solid-npc		#x002 "Solid to NPCs.")
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

(defparameter *tile-attributes-table*
  (map 'vector #'identity (read-tile-key-table))
  "Table of tile-attribute-idx (from a .pxa file) to a list
of *tile-attributes*.")

(defparameter *entity-type-table*
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
(defparameter *tsc-command-table*
  '((:AE+ "Refill ammo")
    (:AM+ "Get weapon X, add Y to max ammo (just adds ammo if you have the weapon)")
    (:AM- "Lose weapon X")
    (:AMJ "Jump to event Y if you have weapon X")
    (:ANP "Animate entity X with method Y in direction Z [entity type determines Y values?]")
    (:BOA "Animate boss entity. Give map-boss scriptstate X.")
    (:BSL "Start boss fight with entity W. Use 0000 to end the boss fight. (NPC flag 0200 must be set; should work with anything that has HP)")
    (:CAT "Instantly display text. Use before a <MSG/2/3; works until <END. Same command as <SAT.")
    (:CIL "Credits: Clear the illustration.")
    (:CLO "Close the text box (used after MSG/MS2/MS3)")
    (:CLR "Clear the text box (used after MSG/MS2/MS3)")
    (:CMP "Change map coords X:Y to tile Z. Produces Smoke.")
    (:CMU "Change music to song X")
    (:CNP "Change all entities X to entity type Y with direction Z")
    (:CPS "Stop propeller sound (used after SPS) (from helicopter cutscene after final battles)")
    (:CRE "Roll credits")
    (:CSS "Stop stream sound (used after SSS) (from River area)")
    (:DNA "Remove all entities of type X.")
    (:DNP "Entity X is removed completely")
    (:ECJ "Jump to event Y if any entity with ID X is present")
    (:END "End scripted event")
    (:EQ+ "Add X to equip flag bytes")
    (:EQ- "Subtract X from equip flag bytes")
    (:ESC "Quit to title screen")
    (:EVE "Jump to event X (non-conditional)")
    (:FAC "Show face X in text box")
    (:FAI "Fade in with direction X")
    (:FAO "Fade out with direction X")
    (:FL+ "Set flag X")
    (:FL- "Clear flag X")
    (:FLA "Flash the screen white")
    (:FLJ "Jump to event Y if flag X is set")
    (:FMU "Fade the music to a low volume (good to use before CMU)")
    (:FOB "Focus on boss X in Y ticks. Use Y > 0.")
    (:FOM "Focus view on you (normal view), view movement takes X ticks (WARNING: speed 0000 crashes)")
    (:FON "Focus view on entity X, view movement takes Y ticks")
    (:FRE "Frees menu cursor [also used after ZAM for some reason?]")
    (:GIT "Show weapon/item X icon above text box - add 1000 to X for items - GIT0000 to hide")
    (:HMC "Removes main character entity (use SMC after)")
    (:INI "Resets memory and starts game from the beginning")
    (:INP "Change entity X to type Y with direction Z and set entity flag 100 (0x8000).")
    (:IT+ "Get item X")
    (:IT- "Lose item X")
    (:ITJ "Jump to event Y if you have item X")
    (:KEY "Hides status bars and locks out input to your character until END (used with MSG/MS2/MS3 and PRI)")
    (:LDP "Loads profile.dat into memory and starts game from save")
    (:LI+ "Restore X amount of health")
    (:ML+ "Max health increased X amount")
    (:MLP "Display map [how is this used without blanking screen while map is displayed?]")
    (:MM0 "Instantly halts your horizontal motion")
    (:MNA "Displays name of current map")
    (:MNP "Move entity X to coords Y:Z facing direction W")
    (:MOV "Move you to coords X:Y")
    (:MP+ "Set a map flag.")
    (:MPJ "Jump to event X if map flag is set for the current area.")
    (:MS2 "Open invisible text box at top of screen (text follows)")
    (:MS3 "Open normal text box at top of screen (text follows)")
    (:MSG "Open normal text box (text follows)")
    (:MYB "Knocks you back from direction X (0000 knocked right, 0002 knocked left, any other just hops in place)")
    (:MYD "Make you face direction X")
    (:NCJ "Jump to event Y if any entity of type X is present")
    (:NOD "Text box wait for button press (used after MSG/MS2/MS3)")
    (:NUM "Prints the value [4a5b34+W*4] to the message box. Use 0000 to print the last used X from compatible commands (eg AM+).")
    (:PRI "Hides status bars and freezes game action until KEY or END (used with MSG/MS2/MS3)")
    (:PS+ "Set teleporter slot X to location Y")
    (:QUA "Shake the screen for X ticks")
    (:RMU "Restore music playback")
    (:SAT "Instant text display on all messages until END (glitches scrolling text)")
    (:SIL "Show illustration during credits (use CIL after)")
    (:SK+ "Set skipflag X (remains set until program exits, to avoid repeating cutscenes/dialogue after retrying)")
    (:SK- "Clear skipflag X")
    (:SKJ "Jump to event Y if skipflag X is set")
    (:SLP "Teleporter location menu")
    (:SMC "Restores main character entity (used after HMC)")
    (:SMP "Jump to event X if skipflag W is set. Does not create smoke.")
    (:SNP "Create an entity of type X at coordinates Y:Z with direction W.")
    (:SOU "Play sound effect X")
    (:SPS "Start propeller sound (use CPS after) (from helicopter cutscene after final battles)")
    (:SSS "Start stream sound at pitch X (use CSS after) (from River area - normal pitch is 0400)")
    (:STC "Saves the current time to 290.rec")
    (:SVP "Save game")
    (:TAM "Trade weapon X for weapon Y, set max ammo to Z (max ammo 0000 = no change) (GLITCH: first weapon 0000)")
    (:TRA "Load map X, run event Y, transport you to coords Z:W")
    (:TUR "Instantly display text. Use after a <MSG/2/3; works until another <MSG/2/3 or an <END.")
    (:UNI "0000 normal / 0001 zero-g movement, facing direction is locked (disables focus commands) (from Stream boss) / 0002 movement is locked, can still fire")
    (:WAI "Pause script for X ticks")
    (:WAS "Pause script until your character touches the ground")
    (:XX1 "Show the island falling in manner W. Use 0000 to have it crash and 0001 to have it stop midway.")
    (:YNJ "Ask yes or no, jump to event X if No")
    (:ZAM "All weapons drop to level 1"))
  "List of (COMMAND-KEY DESCRIPTION) pairs for tsc script <XYZ commands.")


(defparameter *song-names-table*
  '(nil :egg :safety :gameover :gravity :grasstown :meltdown2 :eyesofflame
    :gestation :town :fanfale1 :balrog :cemetary :plant :pulse :fanfale2
    :fanfale3 :tyrant :run :jenka1 :labyrinth :access :oppression
    :geothermal
    :theme :oside :heroend :scorching :quiet :lastcave :balcony :charge
    :lastbattle :credits :zombie :breakdown :hell :jenka2 :waterway :seal
    :toroko :white :azarashi nil)
  "Ordered list of song names; accessed by tsc scripts.")

(defparameter *sound-effects-table*
  '((:snd-menu-move 1)
    (:snd-msg 2)
    (:snd-bonk-head 3)
    (:snd-switch-weapon 4)
    (:snd-menu-prompt 5)
    (:snd-hoppy-jump 6)
    (:snd-door 11)
    (:snd-block-destroy 12)
    (:snd-get-xp 14)
    (:snd-player-jump 15)
    (:snd-player-hurt 16)
    (:snd-player-die 17)
    (:snd-menu-select 18)
    (:snd-health-refill 20)
    (:snd-bubble 21)
    (:snd-chest-open 22)
    (:snd-thud 23)
    (:snd-player-walk 24)
    (:snd-funny-explode 25)
    (:snd-quake 26)
    (:snd-level-up 27)
    (:snd-shot-hit 28)
    (:snd-teleport 29)
    (:snd-enemy-jump 30)
    (:snd-tink 31)
    (:snd-polar-star-l1-2 32)
    (:snd-snake-fire 33)
    (:snd-fireball 34)
    (:snd-explosion1 35)
    (:snd-gun-click 37)
    (:snd-get-item 38)
    (:snd-em-fire 39)
    (:snd-stream1 40)
    (:snd-stream2 41)
    (:snd-get-missile 42)
    (:snd-computer-beep 43)
    (:snd-missile-hit 44)
    (:snd-xp-bounce 45)
    (:snd-ironh-shot-fly 46)
    (:snd-explosion2 47)
    (:snd-bubbler-fire 48)
    (:snd-polar-star-l3 49)
    (:snd-enemy-squeak 50)
    (:snd-enemy-hurt 51)
    (:snd-enemy-hurt-big 52)
    (:snd-enemy-hurt-small 53)
    (:snd-enemy-hurt-cool 54)
    (:snd-enemy-squeak2 55)
    (:snd-splash 56)
    (:snd-enemy-damage 57)
    (:snd-propellor 58)
    (:snd-spur-charge-1 59)
    (:snd-spur-charge-2 60)
    (:snd-spur-charge-3 61)
    (:snd-spur-fire-1 62)
    (:snd-spur-fire-2 63)
    (:snd-spur-fire-3 64)
    (:snd-spur-maxed 65)
    (:snd-expl-small 70)
    (:snd-little-crash 71)
    (:snd-big-crash 72)
    (:snd-bubbler-launch 100)
    (:snd-lightning-strike 101)
    (:snd-jaws 102)
    (:snd-charge-gun 103)
    (:snd-104 104)
    (:snd-puppy-bark 105)
    (:snd-slash 106)
    (:snd-block-move 107)
    (:snd-igor-jump 108)
    (:snd-critter-fly 109)
    (:snd-droll-shot-fly 110)
    (:snd-motor-run 111)
    (:snd-motor-skip 112)
    (:snd-booster 113)
    (:snd-core-hurt 114)
    (:snd-core-thrust 115)
    (:snd-core-charge 116)
    (:snd-nemesis-fire 117)
    (:snd-150 150)
    (:snd-151 151)
    (:snd-152 152)
    (:snd-153 153)
    (:snd-154 154)
    (:snd-155 155))
  "AList of (SOUND-KEY SOUND-IDX).")

(defun sound-effect-idx->sound-effect-key (idx)
  (first (member idx *sound-effects-table* :key #'second)))

(defparameter *directions-table*
  #(:left :up :right :down :center)
  "For TSC directions.
NOTE: For MYB it is 0000 Right, 0002 Left (reversed).")

(defparameter *maps-table*
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

(defparameter *weapons-table*
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

(defparameter *item-table*
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

(defparameter *equip-flags*
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

(defparameter *face-table*
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

(defparameter *illustrations-table*
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

(defparameter *npc-data* (read-npc-table "./content/npc.tbl")
  "Alist read in from the npc table.")
(defparameter *smoke-amounts-table* #(0 3 7 12)
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
	(get-sound (comp first #_(elt *sound-effects-table* _))))
    (aupdate e
	     :smoke-amt #_(elt *smoke-amounts-table* _)
	     :hurt-sound get-sound
	     :death-sound get-sound
	     :default-flags #'pxe-flags->entity-flags)))

(defun pickup-fns-alist ()
  (alist :pickup-rect-fn (lambda (a) (centered-rect (+v (aval a :pos)
							(tile-dims/2))
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
			 (tile-v 0 5)
			 (tile-v 0 7))))))
    (list
     (if (> (aval tm :ms-remaining) (* 2 *frame-time*))
	 (unless (death-flash? tm)
	   (make-sprite-drawing
	    :layer :pickup
	    :sheet-key :npc-sym
	    :src-rect
	    (tile-rect (+v (anim-cycle-offset a) src-pos))
	    :pos (aval a :pos)))
	 (make-sprite-drawing
	  :layer :pickup
	  :sheet-key :npc-sym
	  :src-rect (tile-rect (tile-v 1 0))
	  :pos (aval a :pos))))))