(in-package :cave-story)

(defparameter *stage-subsystems* '(:drawable))

(defun basic-stage ()
  "Just a toy stage for debugging."
  (let ((stage (make-array '(15 40) :initial-element nil))
	(wall (list '(:solid-player :solid-npc :solid-shot :foreground)
		    (make-v 1 0))))
    (dotimes (i 40)
      (setf (aref stage 10 i) wall))
    (setf (aref stage 9 15) wall)

    (setf (aref stage 9 14) (list '(:foreground :slope :rbt)
				  (make-v 5 1)))
    (setf (aref stage 9 13) (list '(:foreground :slope :rbs)
				  (make-v 4 1)))

    #+nil((setf (aref stage 6 10) wall)
	  (setf (aref stage 7 12) wall))
    (setf (aref stage 9 12) wall)

    (let ((x -1))
      (setf (aref stage 9 (incf x)) wall)
      (setf (aref stage 9 (incf x)) (list '(:foreground :slope :lbt)
					  (make-v 2 1)))
      (setf (aref stage 9 (incf x)) (list '(:foreground :slope :lbs)
					  (make-v 3 1)))
      (setf (aref stage 9 (incf x)) (list '(:foreground :slope :rbs)
					  (make-v 4 1)))
      (setf (aref stage 9 (incf x)) (list '(:foreground :slope :rbt)
					  (make-v 5 1)))
      (setf (aref stage 9 (incf x)) wall))
    (let ((x -1))
      (setf (aref stage 7 (incf x)) wall)
      (setf (aref stage 7 (incf x)) (list '(:foreground :slope :ltt)
					  (make-v 2 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (list '(:foreground :slope :lts)
					  (make-v 3 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (list '(:foreground :slope :rts)
					  (make-v 4 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) (list '(:foreground :slope :rtt)
					  (make-v 5 0)))
      (setf (aref stage 6 x) wall)
      (setf (aref stage 7 (incf x)) wall))

    stage))

(defun stage-fns-alist ()
  (alist :draw-fn #_(aval _ :drawings)))

(defun make-stage (stage-data)
  (amerge
   (stage-fns-alist)
   (alist :subsystems *stage-subsystems*)
   (alist :data stage-data
	  :drawings (ncompile-drawings (stage-drawing stage-data))
	  :id (gen-entity-id))))

(defun stage-dims (stage)
  (let ((data (aval stage :data)))
    (tile-v (array-dimension data 1)
	    (array-dimension data 0))))

(defun stage-get-colliding-tiles (stage rect)
  "Return all tiles colliding with rect. ((TILE-POS TILE-TYPE) ...)"
  (let ((data (aval stage :data)))
    (let ((tile-tl (pos->tile-pos (rect-pos rect)))
	  (tile-br (pos->tile-pos (+v (rect-pos rect) (rect-size rect)))))
      (loop named stage-loop
	 ;; For each tile colliding with rect
	 for row
	 from (max 0 (y tile-tl))
	 to (min (1- (array-dimension data 0)) (y tile-br))
	 appending
	   (loop
	      for col
	      from (max 0 (x tile-tl))
	      to (min (1- (array-dimension data 1)) (x tile-br))
	      collecting
		(list (make-v col row) (car (aref data row col))))))))

(defmacro collision-lambda ((&optional data) &body forms)
  (let ((data (if data
		  data
		  (gensym))))
    `(lambda (,data)
       (declare (ignorable ,data))
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

       (cond
	 ((member :solid-player tile-type)
	  (return
	    (values (flush-rect-with-wall rect tile-pos offset-dir)
		    tile-type)))
	 ((member :slope tile-type)
	  (let* ((top-tile? (intersection tile-type '(:ltt :lts :rts :rtt)))
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
    (data stage collision-rects collision-reactions
     &optional (ground-tile nil ground-tile-provided-p))
  "Data is an alist with :stage-physics. It is passed to
and returned from each collision reaction, so that results can be accumulated.
 (:tile-type . tile-type) will be added before calling each collision reaction.
Stage-collisions returns the final data argument."
  (dolist (side *collision-order*)
    (let* ((fn (cdr (assoc side collision-reactions)))
	   (collision-rect (cdr (assoc side collision-rects))))
      (multiple-value-bind (new-pos tile-type)
	  (let ((args (list stage
			    (rect-offset collision-rect
					 (aval (aval data :stage-physics) :pos))
			    (opposite-dir side))))
	    (when ground-tile-provided-p
	      (appendf args (list :ground-tile ground-tile)))
	    (apply #'stage-check/resolve-collision args))
	(when new-pos
	  (setq data (aset
		      data
		      :stage-physics
		      (aset (aval data :stage-physics)
			    :pos (sub-v new-pos (rect-pos collision-rect)))
		      :tile-type tile-type))
	  (when fn
	    (setq data (funcall fn data))))
	(draw-rect! (rect-offset collision-rect (aval (aval data :stage-physics)
						      :pos))
		    *blue*
		    :layer
		    :debug-stage-collision))))
  data)

(defun tile-attributes->color (tile-type)
  (cond ((null tile-type) *white*)
	((member :water tile-type) *cyan*)
	((member :destroyable tile-type) *magenta*)
	((member :hurts-player tile-type) *red*)
	((member :solid-player tile-type) *blue*)
	((member :foreground tile-type) *yellow*)
	(t *red*)))

(defun stage-drawing (stage-data)
  (let ((drawings nil)
	(data stage-data))
    (dotimes (row (array-dimension data 0))
      (dotimes (col (array-dimension data 1))
	(let ((tile-type (first (aref data row col)))
	      (tile-pos (second (aref data row col))))
	  (push
	   (if (member :slope tile-type)
	       (slope-drawing (make-v col row) tile-type *green* :debug)
	       (tile-rect-drawing
		(tile-v col row) (tile-attributes->color tile-type)
		:debug nil))
	   drawings)
	  (when tile-pos
	    (push
	     (make-sprite-drawing
	      :layer (if (member :foreground tile-type)
			 :foreground
			 :background)
	      :sheet-key :prt-cave
	      :src-rect (tile-rect (tile-pos tile-pos))
	      :pos (tile-v col row))
	     drawings)))))
    drawings))