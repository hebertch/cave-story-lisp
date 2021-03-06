(in-package :cave-story)

(defvar* *gun-names*
  #(:polar-star :missile-launcher :machine-gun :fireball :nemesis
    :super-missile-launcher :bubbler :spur :snake))

(defun gun-pos (pos h-facing actual-v-facing walking? walk-idx)
  (let ((gun-x-off (if (eq h-facing :right)
		       0
		       (- (tiles 1/2))))

	(gun-y-off (case actual-v-facing
		     (:up   (tiles -1/4))
		     (:down (tiles  1/4))
		     (t     0)))
	(gun-bob-y-off (if (and walking? (= walk-idx 0))
			   -2
			   0)))
    (+ pos (make-v gun-x-off (+ gun-y-off gun-bob-y-off)))))

(defvar* *gun-x-idxs*
  '(:spur :snake :polar-star :fireball :machine-gun :missile-launcher
    nil :nemesis nil nil :super-missile-launcher nil :bubbler))

(defvar* *nozzle-pixel-positions*
  '((:polar-star . (106 21
		    134 53
		    128 68
		    113 100
		    128 156
		    113 87))
    (:snake . (58 22
	       85 55
	       75 68
	       68 100
	       76 156
	       67 188))
    (:fireball . (154 22
		  182 54
		  169 68
		  166 100
		  172 155
		  163 188))
    (:machine-gun . (198 23
		     233 55
		     217 66
		     215 98
		     221 159
		     211 191))
    (:missile-launcher . (250 17
			  277 49
			  265 66
			  262 98
			  270 157
			  257 189))
    (:nemesis . (342 21
		 377 53
		 362 66
		 358 98
		 364 159
		 356 192))
    (:super-missile-launcher . (490 18
				517 50
				505 66
				502 98
				510 157
				497 189))
    (:bubbler . (581 22
		 617 54
		 601 66
		 598 98
		 605 159
		 595 191))
    (:spur . (8 215
	      40 247
	      26 258
	      21 290
	      29 350
	      19 382)))
  "Positions of the nozzle relative to Arms.BMP 0x0")

(defvar* *gun-width* (tiles 3/2))

(defun gun-sprite-rect (gun-name h-facing v-facing)
  (let ((gun-y-idx (+ (if (eq gun-name :spur) 6 0)
		      (position h-facing '(:left :right))
		      (* 2
			 (position v-facing '(nil :up :down))))))
    (create-rect-cmpts (* (position gun-name *gun-x-idxs*) *gun-width*)
		       (tiles gun-y-idx)
		       *gun-width*
		       (tiles 1))))

(defun nozzle-pixel-positions->nozzle-offsets (gun-name)
  (let ((npp (cdr (assoc gun-name *nozzle-pixel-positions*)))
	(i -1)
	nozzle-offsets)
    (loop for v-facing in '(nil :up :down)
       do
	 (let* ((l-src (gun-sprite-rect gun-name :left v-facing))
		(l-off (- (make-v (elt npp (setq i (1+ i)))
				  (elt npp (setq i (1+ i))))
			  (rect-pos l-src)))
		(r-src (gun-sprite-rect gun-name :right v-facing))
		(r-off (- (make-v (elt npp (setq i (1+ i)))
				  (elt npp (setq i (1+ i))))
			  (rect-pos r-src))))
	   (setq nozzle-offsets
		 (acons v-facing
			(list (list (x l-off) (x r-off))
			      (y l-off))
			nozzle-offsets))))
    nozzle-offsets))

(defun player-gun-drawing (pos gun-name h-facing actual-v-facing walking? walk-idx)
  (make-sprite-drawing :layer :gun
		       :sheet-key :arms
		       :src-rect (gun-sprite-rect gun-name h-facing actual-v-facing)
		       :pos (gun-pos pos h-facing actual-v-facing walking? walk-idx)))

(defvar* *gun-nozzle-offsets*
  (loop for gun-name across *gun-names*
     collecting
       (cons gun-name (nozzle-pixel-positions->nozzle-offsets gun-name))))

(defun gun-offsets (gun-name v-facing)
  (cdr (assoc v-facing
	      (cdr (assoc gun-name *gun-nozzle-offsets*)))))

(defun nozzle-offset (h-facing v-facing gun-name)
  (let* ((offsets (gun-offsets gun-name v-facing))
	 (x-offsets (first offsets))
	 (y-offset (second offsets)))
    (make-v (ecase h-facing
	      (:left (first x-offsets))
	      (:right (second x-offsets)))
	    y-offset)))

(defun player-nozzle-pos (p)
  (let ((actual-v-facing (player-actual-v-facing p))
	(k (aval p :stage-physics)))
    (+ (nozzle-offset (aval p :h-facing)
		      actual-v-facing
		      (player-current-gun-name p))
       (gun-pos
	(aval k :pos)
	(aval p :h-facing)
	actual-v-facing
	(player-walking? p)
	(player-walk-idx p)))))

(defun gun-level (exp exp-list)
  (let ((lvl (position-if-not (lambda (lvl-exp)
				(>= exp lvl-exp))
			      exp-list)))
    (if lvl
	lvl
	2)))

(defvar* *max-projectile-groups*
  ;; TODO: This is based on level.
  '((:polar-star . 2)
    (:missile-launcher . 2)))

;; Polar Star
(defvar* *polar-star-exp* 40)
(defvar* *polar-star-projectile-max-offsets*
  (mapcar #'tiles '(7/2 5 7)))


;; Missile Launcher
(defvar* *missile-projectile-amplitude* 4)
(defvar* *missile-radial-speed* 0.010800001)

(defvar* *gun-level-exps*
  (append '((:polar-star . (10 30 40)))
	  (loop for g across *gun-names*
	     collecting (cons g (list 10 30 40)))))
