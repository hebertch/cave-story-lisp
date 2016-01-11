(in-package :cave-story)

(defparameter *gun-names* #(:polar-star :missile-launcher :machine-gun :fireball :nemesis
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
    (+v pos (make-v gun-x-off (+ gun-y-off gun-bob-y-off)))))

(defparameter *gun-x-idxs* '(:spur :snake :polar-star :fireball :machine-gun :missile-launcher
			     nil :nemesis nil nil :super-missile-launcher nil :bubbler))

(defparameter *nozzle-pixel-positions*
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

(defparameter *gun-width* (tiles 3/2))

(defun gun-sprite-rect (gun-name h-facing v-facing)
  (let ((gun-y-idx (+ (if (eq gun-name :spur) 6 0)
		      (position h-facing '(:left :right))
		      (* 2
			 (position v-facing '(nil :up :down))))))
    (create-rect-cmpts (* (position gun-name *gun-x-idxs*) *gun-width*) (tiles gun-y-idx)
		       *gun-width* (tiles 1))))

(defun nozzle-pixel-positions->nozzle-offsets (gun-name)
  (let ((npp (cdr (assoc gun-name *nozzle-pixel-positions*)))
	(i -1)
	nozzle-offsets)
    (loop for v-facing in '(nil :up :down)
       do
	 (let* ((l-src (gun-sprite-rect gun-name :left v-facing))
		(l-off (sub-v (make-v (elt npp (incf i))
				      (elt npp (incf i)))
			      (rect-pos l-src)))
		(r-src (gun-sprite-rect gun-name :right v-facing))
		(r-off (sub-v (make-v (elt npp (incf i))
				      (elt npp (incf i)))
			      (rect-pos r-src))))
	   (setf nozzle-offsets
		 (acons v-facing
			(list (list (x l-off) (x r-off))
			      (y l-off))
			nozzle-offsets))))
    nozzle-offsets))

(defun player-draw-gun (pos gun-name h-facing actual-v-facing walking? walk-idx)
  (draw-sprite :gun :arms
	       (gun-sprite-rect gun-name h-facing actual-v-facing)
	       (gun-pos pos h-facing actual-v-facing walking? walk-idx)))

(defparameter *gun-nozzle-offsets*
  (loop for gun-name across *gun-names*
     collecting
       (cons gun-name (nozzle-pixel-positions->nozzle-offsets gun-name))))

(defun nozzle-offset (h-facing v-facing gun-name)
  (let* ((offsets (cdr (assoc v-facing (cdr (assoc gun-name *gun-nozzle-offsets*)))))
	 (x-offsets (first offsets))
	 (y-offset (second offsets)))
    (make-v (ecase h-facing
	      (:left (first x-offsets))
	      (:right (second x-offsets)))
	    y-offset)))

(defun player-nozzle-pos (p)
  (with-player-slots (p)
    (let* ((on-ground? (player-on-ground? ground-tile))
	   (actual-v-facing (player-actual-v-facing v-facing on-ground?)))
      (let ((k (cdr (assoc :stage physics))))
	(+v (nozzle-offset h-facing
			   actual-v-facing
			   (player-current-gun-name gun-name-cycle))
	    (gun-pos
	     (kin-2d-pos k)
	     h-facing
	     actual-v-facing
	     (player-walking? acc-dir on-ground?)
	     (player-walk-idx p)))))))

(defun gun-level (exp exp-list)
  (aif (position-if-not (lambda (lvl-exp)
			  (>= exp lvl-exp))
			exp-list)
       it
       2))

(defparameter *max-projectile-groups*
  ;; TODO: This is based on level.
  '((:polar-star . 2)
    (:missile-launcher . 2)))

;; Polar Star
(defparameter *polar-star-exp* 40)
(defparameter *polar-star-projectile-max-offsets*
  (mapcar #'tiles '(7/2 5 7)))


;; Missile Launcher
(defparameter *missile-projectile-amplitude* 4)
(defparameter *missile-radial-speed* 0.010800001)

(defparameter *gun-level-exps*
  (append '((:polar-star . (10 30 40)))
	  (loop for g across *gun-names*
	     collecting (cons g (list 10 30 40)))))
