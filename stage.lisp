(in-package :cave-story)

(defmethod draw ((s array))
  (stage-drawing s))

(defun create-stage! (stage-data)
  (create-entity
   stage-data
   '(:drawable)))

(defun basic-stage ()
  "Just a toy stage for debugging."
  (let ((stage (make-array '(15 40) :initial-element nil))
	(wall (cons :wall (make-v 1 0))))
    (dotimes (i 40)
      (setf (aref stage 10 i) wall))
    (setf (aref stage 9 15) wall)

    (setf (aref stage 9 14) (cons :rbt (make-v 5 1)))
    (setf (aref stage 9 13) (cons :rbs (make-v 4 1)))

    #+nil((setf (aref stage 6 10) wall)
	  (setf (aref stage 7 12) wall))
    (setf (aref stage 9 12) wall)

    (let ((x -1))
      (setf (aref stage 9 (incf x)) wall)
      (setf (aref stage 9 (incf x)) (cons :lbt (make-v 2 1)))
      (setf (aref stage 9 (incf x)) (cons :lbs (make-v 3 1)))
      (setf (aref stage 9 (incf x)) (cons :rbs (make-v 4 1)))
      (setf (aref stage 9 (incf x)) (cons :rbt (make-v 5 1)))
      (setf (aref stage 9 (incf x)) wall))
    (let ((x -1))
      (setf (aref stage 7 (incf x)) wall)
      (setf (aref stage 7 (incf x)) (cons :ltt (make-v 2 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (cons :lts (make-v 3 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (cons :rts (make-v 4 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (cons :rtt (make-v 5 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) wall))

    stage))

(defun stage-dims (stage)
  (tile-v (array-dimension stage 1)
	  (array-dimension stage 0)))

(defun stage-get-colliding-tiles (stage rect)
  "Return all tiles colliding with rect. ((TILE-POS TILE-TYPE) ...)"
  (let ((tile-tl (pos->tile-pos (rect-pos rect)))
	(tile-br (pos->tile-pos (+v (rect-pos rect) (rect-size rect)))))
    (loop named stage-loop
       ;; For each tile colliding with rect
       for row
       from (max 0 (y tile-tl))
       to (min (1- (array-dimension stage 0)) (y tile-br))
       appending
	 (loop
	    for col
	    from (max 0 (x tile-tl))
	    to (min (1- (array-dimension stage 1)) (x tile-br))
	    collecting
	      (list (make-v col row) (car (aref stage row col)))))))

(defmacro collision-lambda ((&optional tile-type-name) &body forms)
  (let ((tile-type-name (if tile-type-name
			    tile-type-name
			    (gensym))))
    `(lambda (,tile-type-name)
       (declare (ignorable ,tile-type-name))
       ,@forms)))

(defun stage-check/resolve-collision
    (stage rect offset-dir &key (ground-tile nil ground-tile-supplied-p))
  "Returns (VALUES POS TILE-TYPE)
Returns a NIL if no collision, otherwise returns a
valid POS offset in direction OFFSET-DIR.
Returns the TILE-TYPE of the colliding tile."

  (loop for (tile-pos tile-type) in (stage-get-colliding-tiles stage rect)
     do
       (draw-tile-rect! (tile-pos tile-pos) *cyan* :layer :debug-stage-collision)

       (case tile-type
	 (:wall
	  (return
	    (values (flush-rect-with-wall rect tile-pos offset-dir)
		    tile-type)))
	 ((:lbt :lbs :rbs :rbt :ltt :lts :rts :rtt)
	  (let* ((top-tile? (member tile-type '(:ltt :lts :rts :rtt)))
		 ;; Test top tiles when going up,
		 ;; and bottom tiles when going down.
		 (should-test? (or (and top-tile? (eq offset-dir :down))
				   (and (not top-tile?) (eq offset-dir :up)))))

	    (when should-test?
	      ;; Only test if our center is inside the tile.
	      (when (rect-center-in-tile? rect tile-pos offset-dir)
		(draw-slope! tile-pos tile-type)
		(let* ((x (x (center rect)))
		       (y (tile-slope-pos-y tile-pos tile-type x))
		       ;; Sticky collisions only apply when going down.
		       (sticky? (and ground-tile-supplied-p
				     (not top-tile?)
				     (sticky-collision?
				      ground-tile
				      tile-type
				      offset-dir))))

		  (when (or sticky?
			    (rect-slope-collision? rect x y offset-dir))
		    (return (values (flush-rect-pos rect y offset-dir)
				    tile-type)))))))))))

(defun stage-collisions
    (position stage collision-rects collision-reactions
     &optional (ground-tile nil ground-tile-provided-p))
  (dolist (side *collision-order*)
    (let* ((fn (cdr (assoc side collision-reactions)))
	   (collision-rect (cdr (assoc side collision-rects))))
      (multiple-value-bind (new-pos tile-type)
	  (let ((args (list stage
			    (rect-offset collision-rect position)
			    (opposite-dir side))))
	    (when ground-tile-provided-p
	      (appendf args (list :ground-tile ground-tile)))
	    (apply #'stage-check/resolve-collision args))
	(when new-pos
	  (setq position (sub-v new-pos (rect-pos collision-rect)))
	  (when fn
	    (funcall fn tile-type)))
	(draw-rect! (rect-offset collision-rect position)
		    *blue*
		    :layer
		    :debug-stage-collision))))
  position)

(defun stage-drawing (stage)
  (let ((drawings nil))
    (dotimes (row (array-dimension stage 0))
      (dotimes (col (array-dimension stage 1))
	(when (aref stage row col)
	  (push
	   (make-sprite-drawing :layer :foreground :sheet-key :prt-cave
				:src-rect
				(tile-rect (tile-pos (cdr (aref stage row col))))
				:pos (tile-v col row))
	   drawings))))
    drawings))
