(in-package :cave-story)

(defparameter *collision-order* '(:bottom :left :right :top))

(defun vertical? (dir) (member dir '(:up :down)))
(defun horizontal? (dir) (member dir '(:left :right)))

(defun opposite-dir (dir)
  (ecase dir
    (:left :right)
    (:right :left)
    ((:top :up) :down)
    ((:bottom :down) :up)))

(defun flush-rect-pos (rect scalar-pos offset-dir)
  "Return a pos of RECT moved in OFFSET-DIR so that it's flush with SCALAR-POS"
  (let ((pos (rect-pos rect))
	(size (rect-size rect)))
    (ecase offset-dir
      (:left
       (make-v (- scalar-pos (x size)) (y pos)))
      (:right
       (make-v scalar-pos (y pos)))
      (:up
       (make-v (x pos) (- scalar-pos (y size))))
      (:down
       (make-v (x pos) scalar-pos)))))

(defun flush-rect-with-wall (rect tile-pos offset-dir)
  "Return a pos of RECT moved in OFFSET-DIR so that it's flush with the wall at ROW, COL"
  (ecase offset-dir
    (:left  (flush-rect-pos rect (tiles (x tile-pos))      offset-dir))
    (:right (flush-rect-pos rect (tiles (1+ (x tile-pos))) offset-dir))
    (:up    (flush-rect-pos rect (tiles (y tile-pos))      offset-dir))
    (:down  (flush-rect-pos rect (tiles (1+ (y tile-pos))) offset-dir))))

(defun tile-type-slope-type (tile-type)
  (car (intersection tile-type '(:lbt :rts :lbs :rtt :rbs :ltt :rbt :lts))))

(defun tile-type-offset (tile-type)
  (ecase (tile-type-slope-type tile-type)
    ((:lbt :rts) 0)
    ((:lbs :rtt) (tiles 1/2))
    ((:rbs :ltt) (tiles 1))
    ((:rbt :lts) (tiles 1/2))))

(defun tile-type-slope (tile-type)
  (ecase (tile-type-slope-type tile-type)
    ((:lbt :rts) 1/2)
    ((:lbs :rtt) 1/2)
    ((:rbs :ltt) -1/2)
    ((:rbt :lts) -1/2)))

(defun tile-slope-pos-y (tile-pos tile-type x)
  "Returns the y pos of the tile."
  (let* ((pos (tile-pos->pos tile-pos))
	 (x-off (- x (x pos))))
    ;; y = slope*(x - pos-x) + pos-y + tile-y-offset
    (+ (y pos) (* (tile-type-slope tile-type) x-off) (tile-type-offset tile-type))))

(defun tile-slope-pos-x (tile-pos tile-type y)
  "Returns the x pos of the tile."
  ;; x = (y - pos-y - tile-y-offset) / slope + pos-x;
  (let ((pos (tile-pos->pos tile-pos)))
    (+ (x pos) (/ (- y (y pos) (tile-type-offset tile-type))
		  (tile-type-slope tile-type)))))

(defun rect-slope-collision? (rect slope-x slope-y offset-dir)
  "Should a rect slope collision occur?"
  (ecase offset-dir
    (:up    (>= (bottom rect) slope-y))
    (:down  (<= (top rect)    slope-y))
    (:left  (>= (right rect)  slope-x))
    (:right (<= (left rect)   slope-x))))

(defun flush-rect-with-slope (rect tile-pos tile-type offset-dir)
  "Return a pos of RECT moved in OFFSET-DIR so that it's flush with the slope tile."
  (let* ((x (+ (x (rect-pos rect))
	       (* 1/2 (x (rect-size rect)))))
	 (y (tile-slope-pos-y
	     tile-pos
	     tile-type
	     x)))
    (when (rect-slope-collision? rect x y offset-dir)
      (flush-rect-pos rect y offset-dir))))

(defun slope? (tile-type)
  (member :slope tile-type))
(defun tall-slope? (tile-type)
  (intersection tile-type '(:ltt :rtt :lbt :rbt)))
(defun short-slope? (tile-type)
  (intersection tile-type '(:lts :rts :lbs :rbs)))
(defun wall? (tile-type)
  (intersection (ensure-list tile-type) '(:solid-player)))

(defun top-slope? (tile-type)
  (intersection tile-type '(:ltt :lts :rts :rtt)))
(defun bottom-slope? (tile-type)
  (intersection tile-type '(:lbt :lbs :rbs :rbt)))
(defun right-slope? (tile-type)
  (intersection tile-type '(:rts :rtt :rbs :rbt)))
(defun left-slope? (tile-type)
  (intersection tile-type '(:lts :ltt :lbs :lbt)))

(defun sticky-collision? (ground-tile tile-type offset-dir)
  "Should a sticky collision be applied?"

  ;; Sticky when going from Slope->Slope Or Wall->Tall-Slope Or Short-Slope->Wall
  (and (eq offset-dir :up)
       (or (and ground-tile
		(slope? ground-tile))
	   (and (wall? ground-tile)
		(tall-slope? tile-type))
	   (and (short-slope? ground-tile)
		(wall? tile-type)))))

(defun rect-center-in-tile? (rect tile-pos offset-dir)
  "Checks to see if RECT's center along OFFSET-DIR is w/in the TILE-POS."
  (if (vertical? offset-dir)
      (<= (tiles (x tile-pos)) (x (center rect)) (tiles (1+ (x tile-pos))))
      (<= (tiles (y tile-pos)) (y (center rect)) (tiles (1+ (y tile-pos))))))
