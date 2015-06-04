(in-package :cave-story)

(defstruct player
  (h-facing :left)
  v-facing
  (walk-cycle (create-cycle :fps 12
			    :data '(0 1 0 2)))
  interacting?
  (pos (make-v 320 240))
  (vel (zero-v))
  acc-dir
  ground-tile
  jumping?

  (invincible-time-remaining 0)

  (current-gun-name :polar-star))

(defparameter player-walk-acc 0.00083007812)
(defparameter player-max-speed-x 0.15859375)
(defparameter player-friction-acc 0.00049804687)
(defparameter terminal-speed 0.2998046875)
(defparameter gravity-acc 0.00078125)
(defparameter player-jump-gravity-acc 0.0003125)
(defparameter player-air-acc 0.0003125)
(defparameter player-jump-speed 0.25)

(defparameter player-collision-rectangles-alist
  (loop for (key x y w h) in
       '((:bottom 11 16 10 15)
	 (:top 7 2 18 15)
	 (:left 6 10 10 12)
	 (:right 16 10 10 12))
     collect (cons key (make-rect :pos (make-v x y) :size (make-v w h)))))

(defun player-collision-rect (side)
  (cdr (assoc side player-collision-rectangles-alist)))

(defparameter player-damage-rect
  (let ((left (left (player-collision-rect :left)))
	(right (right (player-collision-rect :right)))
	(top (top (player-collision-rect :top)))
	(bottom (bottom (player-collision-rect :bottom))))
    (make-rect :pos (make-v left top)
	       :size (make-v (- right left)
			     (- bottom top)))))

(defun char-sprite-tile-pos (h-facing v-facing interacting? walk-idx)
  "Grabs the tile-pos for the character given the character's state."
  (let ((row (ecase h-facing
	       (:left 0)
	       (:right 1))))
    (if interacting?
	(make-v 7 row)
	(let ((col (case v-facing
		     (:up (+ 3 walk-idx))
		     (:down 6)
		     (t
		      (assert (<= 0 walk-idx 2))
		      walk-idx))))
	  (make-v col row)))))

(defun player-on-ground? (p)
  (player-ground-tile p))

(defun player-actual-v-facing (p)
  "The player cannot actually face down when on the ground."
  (let ((vf (player-v-facing p)))
    (if (and (player-on-ground? p)
	     (eq vf :down))
	nil
	vf)))

(defun player-walk-idx (player)
  (car (cycle-current (player-walk-cycle player))))

(defun player-walking? (player)
  (and (player-acc-dir player) (player-on-ground? player)))

(defun player-sprite-rect (p)
  "The sprite-rect for player P."
  (tile-rect (char-sprite-tile-pos
	      (player-h-facing p)
	      (player-actual-v-facing p)
	      (player-interacting? p)
	      (cond
		((player-walking? player)
		 (player-walk-idx p))
		((plusp (y (player-vel player))) 1)
		((minusp (y (player-vel player))) 2)
		(t 0)))))


(defun player-jump (player)
  (unless (player-jumping? player)
    (when (player-on-ground? player)
      (setf (y (player-vel player)) (- player-jump-speed))
      (push (make-sound :key :jump) sfx-play-list))
    (nilf (player-interacting? player)
	  (player-ground-tile player))
    (tf (player-jumping? player))))

(defun player-move (player dir)
  "Moves the player in a horizontal direction."
  (when (and (not (player-acc-dir player))
	     (player-on-ground? player))
    (cycle-reset (player-walk-cycle player)))
  (setf (player-acc-dir player) dir)
  (setf (player-h-facing player) dir)
  (nilf (player-interacting? player)))

(defun player-input (player input)
  (let ((left?  (key-held? input :left))
	(right? (key-held? input :right))
	(down?  (key-held? input :down))
	(up?    (key-held? input :up)))
    (cond
      ;; Look Up/Down or Interact
      ((and up? (not down?))
       (nilf (player-interacting? player))
       (setf (player-v-facing player) :up))
      ((and down? (not up?))
       (unless (eq (player-v-facing player) :down)
	 (setf (player-v-facing player) :down)
	 (setf (player-interacting? player)
	       (player-on-ground? player))))
      (t (nilf (player-v-facing player))))
    (cond
      ;; Walk/Look based on horizontal direction
      ((and left? (not right?))
       (player-move player :left))
      ((and right? (not left?))
       (player-move player :right))
      (t
       (when (player-walking? player)
	 (push (make-sound :key :step)
	       sfx-play-list))
       (nilf (player-acc-dir player))))


    (if (key-held? input :z)
	(player-jump player)
	(nilf (player-jumping? player)))))

(defun player-physics (player)
  ;; Vertical motion
  (let ((acc (if (and (minusp (y (player-vel player)))
		      (player-jumping? player))
		 player-jump-gravity-acc
		 gravity-acc)))
    (mvbind (posp vp) (accelerate (y (player-pos player)) (y (player-vel player)) acc)
      (setf (y (player-pos player)) posp
	    (y (player-vel player)) vp)))

  ;; Horizontal motion
  (cond
    ((and (player-on-ground? player)
	  (null (player-acc-dir player)))
     ;; Not accelerating, apply friction.
     (mvbind (posp vp) (friction-accelerate (x (player-pos player)) (x (player-vel player)) player-friction-acc)
       (setf (x (player-pos player)) posp
	     (x (player-vel player)) vp)))
    (t
     ;; Apply walking accelerations.
     (let ((acc
	    (let ((move-acc (if (player-on-ground? player)
				player-walk-acc
				player-air-acc)))
	      (case (player-acc-dir player)
		(:left (- move-acc))
		(:right move-acc)
		(t 0)))))
       (mvbind (posp vp) (accelerate (x (player-pos player)) (x (player-vel player)) acc)
	 (setf (x (player-pos player)) posp
	       (x (player-vel player)) vp)))))

  ;; Clamp the Player's velocity
  (setf (player-vel player)
	(make-v (clamp (x (player-vel player)) (- player-max-speed-x) player-max-speed-x)
		(clamp (y (player-vel player)) (- terminal-speed) terminal-speed)))
  (push-debug-render (make-line-drawing :color #(255 0 255 255)
					:a (player-pos player)
					:b (add-v (player-pos player)
						  (scale-v (player-vel player) 500)))))

(defun player-collisions (player stage)
  (flet ((collision-check (side dir react-fn rect-color)
	   (let* ((collision-rect (player-collision-rect side))
		  (rect (rect-offset collision-rect (player-pos player))))
	     (when rect-color
	       (push-debug-render (make-rect-drawing
				   :color rect-color
				   :rect rect
				   :filled? t)))
	     (mvbind (new-pos tile-type) (stage-check/resolve-sticky-collision
					  stage
					  rect
					  dir
					  (player-ground-tile player))
	       (when new-pos
		 (setf (player-pos player) (sub-v new-pos (rect-pos collision-rect)))
		 (funcall react-fn tile-type)))))
	 (stop-x (&optional tile-type)
	   (declare (ignore tile-type))
	   (setf (x (player-vel player)) 0)))

    ;; Bottom side
    (let (ground-tile)
      (collision-check
       :bottom :up
       (lambda (&optional tile-type)
	 (setf (y (player-vel player)) 0)
	 (unless (player-ground-tile player)
	   (push (make-sound :key :land)
		 sfx-play-list))
	 (setf ground-tile tile-type))
       #(255 0 0 255))
      (if ground-tile
	  (setf (player-ground-tile player) ground-tile)
	  (nilf (player-ground-tile player))))

    ;; Left
    (collision-check
     :left :right
     #'stop-x
     #(255 0 255 255))

    ;; Right
    (collision-check
     :right :left
     #'stop-x
     #(0 0 255 255))

    ;; Top
    (collision-check
     :top :down
     (lambda (&optional tile-type)
       (declare (ignore tile-type))
       (when (minusp (y (player-vel player)))
	(push (make-sound :key :head-bump) sfx-play-list))
       (setf (y (player-vel player)) (max 0 (y (player-vel player)))))
     #(255 255 255 255))))

(defun player-draw (player)
  (unless (and (plusp (player-invincible-time-remaining player))
	       (not (= (mod (/ (player-invincible-time-remaining player) 50) 2)
		       0)))
    (push-render (make-sprite-drawing
		  :layer :player
		  :sheet-key :my-char
		  :src-rect (player-sprite-rect player)
		  :pos (pixel-v (player-pos player))))
    (player-draw-gun player)))

(defun player-damage-collision-rect (p)
  (let ((r (rect-offset player-damage-rect (player-pos p))))
    (push-debug-render (make-rect-drawing
			:color #(0 0 255 255)
			:filled? t
			:rect r))
    r))
