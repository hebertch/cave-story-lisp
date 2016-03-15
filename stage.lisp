(in-package :cave-story)

(defvar* *stage-subsystems* '(:drawable))

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
  (alist :draw-fn #'stage-drawings))

(defun stage-drawings (stage)
  (append (aval stage :drawings)
	  (destroyable-tile-drawings stage)))

(defun load-stage-from-stage-key (stage-key)
  (let* ((stage-fnames (aval *stage-fnames-table* stage-key))
	 (data
	  (stage-from-file-data
	   (read-pxm-file (format nil "./content/stages/~A.pxm"
				  (aval stage-fnames :stage)))
	   (read-pxa-file (format nil "./content/stages/~A.pxa"
				  (aval stage-fnames :attributes))))))
    (amerge
     (stage-fns-alist)
     (alist :subsystems *stage-subsystems*)
     (alist :data data
	    :drawings (ncompile-drawings
		       (prerendered-stage-drawings
			data
			(aval stage-fnames :texture)))
	    :id (gen-entity-id)))))

(defun stage-dims (stage)
  (let ((data (aval stage :data)))
    (tile-v (array-dimension data 1)
	    (array-dimension data 0))))

(defun stage-get-colliding-tiles (stage rect)
  "Return all tiles colliding with rect. ((TILE-POS TILE-TYPE) ...)"
  (let ((data (aval stage :data)))
    (let ((tile-tl (pos->tile-pos (rect-pos rect)))
	  (tile-br (pos->tile-pos (+ (rect-pos rect) (rect-size rect)))))
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
    (stage rect offset-dir ground-tile ground-tile-supplied-p)
  "Returns (VALUES POS TILE-TYPE)
Returns a NIL if no collision, otherwise returns a
valid POS offset in direction OFFSET-DIR.
Returns the TILE-TYPE of the colliding tile."

  (loop for (tile-pos tile-type) in (stage-get-colliding-tiles stage rect)
     do
       (draw-tile-rect (tile-pos tile-pos) *cyan* :layer :debug-stage-collision)

       (cond
	 ((member :solid-player tile-type)
	  (return
	    (values (flush-rect-with-wall rect tile-pos offset-dir)
		    tile-type)))
	 ((slope? tile-type)
	  (let (;; Test top tiles if going up,
		;; and bottom tiles if going down.
		(should-test? (or (and (top-slope? tile-type)
				       (eq offset-dir :down))
				  (and (bottom-slope? tile-type)
				       (eq offset-dir :up)))))

	    (when should-test?
	      ;; Only test if our center is inside the tile.
	      (when (rect-center-in-tile? rect tile-pos offset-dir)
		(draw-slope tile-pos tile-type)
		(let* ((x (x (center rect)))
		       (y (tile-slope-pos-y tile-pos tile-type x))
		       ;; Sticky collisions only apply when going down.
		       (sticky? (and ground-tile-supplied-p
				     (bottom-slope? tile-type)
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
	  (let* ((pos (aval (aval data :stage-physics) :pos))
		 (rect (rect-offset collision-rect pos)))
	    (stage-check/resolve-collision
	     stage rect (opposite-dir side) ground-tile ground-tile-provided-p))
	(when new-pos
	  (setq data (aupdate data
			      :stage-physics
			      (asetfn :pos
				      (- new-pos (rect-pos collision-rect)))
			      :tile-type (constantly tile-type)))
	  (when fn
	    (setq data (funcall fn data))))
	(draw-rect (rect-offset collision-rect
				 (aval (aval data :stage-physics) :pos))
		    *blue*
		    :layer :debug-stage-collision))))
  data)

(defun tile-attributes->color (tile-type)
  (cond ((null tile-type) *white*)
	((member :water tile-type) *cyan*)
	((member :destroyable tile-type) *magenta*)
	((member :hurts-player tile-type) *red*)
	((member :solid-player tile-type) *blue*)
	((member :foreground tile-type) *yellow*)
	(t *red*)))

(defun destroyable-tile-drawings (stage)
  (let ((drawings nil)
	(data (aval stage :data)))
    (dotimes (row (array-dimension data 0))
      (dotimes (col (array-dimension data 1))
	(let ((tile-type (first (aref data row col))))
	  (when (destroyable? tile-type)
	    (push
	     (make-sprite-drawing
	      :layer :foreground
	      :sheet-key :npc-sym
	      :src-rect (tile-rect (tile-v 16 3))
	      :pos (tile-v col row))
	     drawings)))))
    drawings))

(defun prerendered-stage-drawings (stage-data spritesheet-key)
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
	  (when (and tile-pos (not (destroyable? tile-type)))
	    (push
	     (make-sprite-drawing
	      :layer (if (member :foreground tile-type)
			 :foreground
			 :background)
	      :sheet-key spritesheet-key
	      :src-rect (tile-rect (tile-pos tile-pos))
	      :pos (tile-v col row))
	     drawings)))))
    drawings))

(defun destroyable? (tile-type)
  (intersection (ensure-list tile-type) '(:destroyable)))

(defun destroy-tile (stage tile-pos)
  (let ((tiles (copy-array (aval stage :data))))
    (setf (aref tiles (y tile-pos) (x tile-pos)) nil)
    (aupdate stage
	     :data (constantly tiles)
	     :sound-effects
	     (pushfn :snd-little-crash)
	     :new-states
	     (appendfn (make-num-death-cloud-particles
			3
			(+ (tile-pos tile-pos) (tile-dims/2)))))))

(defun stage-tile-shot (stage tile)
  "Stage reaction to a tile being shot."
  (let ((tile-type (second tile))
	(tile-pos (first tile)))
    (if (destroyable? tile-type)
	(destroy-tile stage tile-pos)
	stage)))
