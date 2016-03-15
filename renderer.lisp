(in-package :cave-story)

(defun drawing-texture-rect (d)
  (make-rect :pos (aval d :pos)
	     :size (rect-size (aval d :src-rect))))

(defun make-sprite-drawing (&key layer sheet-key src-rect pos)
  (alist :rect-fn #'drawing-texture-rect
	 :layer layer
	 :sheet-key sheet-key
	 :src-rect src-rect
	 :pos pos
	 :type :sprite))

(defun make-texture-drawing (&key layer texture src-rect pos)
  (alist :rect-fn #'drawing-texture-rect
	 :layer layer
	 :texture texture
	 :src-rect src-rect
	 :pos pos
	 :type :texture))

(defun make-rect-drawing (&key layer color rect filled?)
  (alist :rect-fn #_(aval _ :rect)
	 :layer layer
	 :color color
	 :rect rect
	 :filled? filled?
	 :type :rect))

(defun make-line-drawing (&key layer color a b)
  (alist :rect-fn (lambda (d) (rect-from-two-points (aval d :a)
						    (aval d :b)))
	 :layer layer
	 :color color
	 :a a :b b
	 :type :line))

(defun make-text-line-drawing (&key layer pos text)
  (alist :rect-fn (lambda (d)
		    (make-rect :pos (aval d :pos)
			       :size (get-text-size (aval *env* :font)
						    (aval d :text))))
	 :layer layer
	 :pos pos
	 :text text
	 :type :text))


(defun drawing-layer (drawing)
  (aval drawing :layer))

(defvar* *red* #(255 0 0 255))
(defvar* *green* #(0 255 0 255))
(defvar* *blue* #(0 0 255 255))
(defvar* *white* #(255 255 255 255))
(defvar* *black* #(0 0 0 255))
(defvar* *yellow* #(255 255 0 255))
(defvar* *magenta* #(255 0 255 255))
(defvar* *cyan* #(0 255 255 255))

(defvar *debug-render-list* nil
  "A separate render-list specifically for debug drawings.")
(defvar* *debug-layers*
    (list :debug
	  :debug-damage-collision
	  :debug-stage-collision
	  :debug-pickup
	  :debug-damageable
	  :debug-dynamic-collision
	  :debug-camera
	  :debug-mouse))
(defvar* *game-layers*
  (list  :background
	 :npc
	 :gun
	 :enemy
	 :pickup
	 :player
	 :projectile
	 :foreground
	 :particle
	 :floating-text))
(defvar* *hud-layers*
  (list :hud-bg :hud :hud-fg
	:text-box :text
	:mouse))
(defvar* *debug-screen-layers*
  (list :debug-mouse))
(defvar* *layers* (append *game-layers* *debug-layers*
			       *hud-layers* *debug-screen-layers*))
(defvar* *screen-layers* (member (first *hud-layers*) *layers*))
(defvar* *visible-layers* (append *game-layers* *hud-layers*))
(defvar* *parallax-scale* 1/8)
(defvar* *character-textures* (make-hash-table))

(defun set-draw-color! (renderer c)
  "Sets the sdl:renderer draw color. c: #(r g b a)"
  (sdl:set-render-draw-color renderer
			     (aref c 0)
			     (aref c 1)
			     (aref c 2)
			     (aref c 3)))

(defun rect->sdl-rect (r)
  "Transforms a CAVE-STORY:RECT into an SDL:RECT"
  (sdl:make-rect (round (x (rect-pos r)))
		 (round (y (rect-pos r)))
		 (round (x (rect-size r)))
		 (round (y (rect-size r)))))

(defun render-texture! (renderer drawing camera-pos)
  "Renderer func. Renders a sprite."
  (let ((src (aval drawing :src-rect))
	(pos (- (aval drawing :pos) camera-pos)))
    (let ((src-pos (rect-pos src))
	  (size (rect-size src)))
      (sdl:render-texture
       renderer
       (aval drawing :texture)
       (sdl:make-rect (x src-pos) (y src-pos) (x size) (y size))
       (sdl:make-rect (round (x pos)) (round (y pos)) (x size) (y size))))))

(defun render-sprite! (renderer drawing camera-pos)
  "Renderer func. Renders a sprite."
  (render-texture!
   renderer
   (aset drawing
	 :texture
	 (get-resource :texture (aval drawing :sheet-key)))
   camera-pos))

(defun render-rect! (renderer rect-drawing camera-pos)
  "Renderer func. Renders a rect."
  (set-draw-color! renderer (aval rect-drawing :color))
  (let* ((rect (aval rect-drawing :rect))
	 (pos (- (rect-pos rect) camera-pos))
	 (size (rect-size rect)))
    (sdl:render-rect renderer
		     (sdl:make-rect (round (x pos))
				    (round (y pos))
				    (round (x size))
				    (round (y size)))
		     :filled? (aval rect-drawing :filled?))))

(defun render-line! (renderer ld camera-pos)
  "Renderer func. Renders a line."
  (set-draw-color! renderer (aval ld :color))
  (let ((a (- (aval ld :a) camera-pos))
	(b (- (aval ld :b) camera-pos)))
    (sdl:render-draw-line renderer
			  (round (x a))
			  (round (y a))
			  (round (x b))
			  (round (y b)))))

(defun push-debug-render (r)
  "Debug method to add a drawing to the debug render-list."
  (push r *debug-render-list*))

(defun slope-drawing (tile-pos tile-type color layer)
  (let* ((pos (tile-pos->pos tile-pos))
	 (left (x pos))
	 (right (+ left *tile-size*)))
    (make-line-drawing
     :layer layer
     :color color
     :a (make-v left (tile-slope-pos-y tile-pos tile-type left))
     :b (make-v right (tile-slope-pos-y tile-pos tile-type right)))))

(defun draw-slope (tile-pos tile-type &key (layer :debug) (color *white*))
  "Pushes a slope to the DEBUG-RENDER-LIST"
  (push-debug-render (slope-drawing tile-pos tile-type color layer)))

(defun pixel-v (v)
  (make-v (round (x v))
	  (round (y v))))

(defun remove-visible-layer! (layer)
  (setq *visible-layers* (set-difference *visible-layers* (list layer))))

(defun add-visible-layer! (layer)
  (pushnew layer *visible-layers*))

(defun toggle-visible-layer! (layer)
  (if (member layer *visible-layers*)
      (remove-visible-layer! layer)
      (add-visible-layer! layer)))

(defun layer< (a b)
  (< (position a *layers*) (position b *layers*)))

(defun nsort-by-layer (drawings)
  (sort drawings #'layer< :key #'drawing-layer))

(defun remove-invisible-layers (drawings)
  (remove-if-not
   #_(member _ *visible-layers*)
   drawings
   :key #'drawing-layer))

(defun draw-rect (rect color &key (layer :debug) filled?)
  "Add a rectangle to the debug draw list."
  (push-debug-render
   (make-rect-drawing :color color
		      :rect rect
		      :layer layer
		      :filled? filled?)))

(defun tile-rect-drawing (pos color layer filled?)
  (make-rect-drawing :color color
		     :rect (tile-rect pos)
		     :layer layer
		     :filled? filled?))

(defun draw-tile-rect (pos color &key (layer :debug) filled?)
  "Add a tile-rectangle to the debug drawing list."
  (push-debug-render (tile-rect-drawing pos color layer filled?)))

(defun point-drawing (pos color layer size)
  (make-rect-drawing :rect (centered-rect pos (both-v size))
		     :color color
		     :layer layer
		     :filled? t))

(defun draw-point (pos color &key (layer :debug) (size 5))
  "Add a point drawing to the debug render list."
  (push-debug-render (point-drawing pos color layer size)))

(defun draw-line (a b color &key (layer :debug))
  "Add a line drawing to the debug render list."
  (push-debug-render
   (make-line-drawing :a a :b b :layer layer :color color)))

(defun get-text-size (font text)
  (destructuring-bind (w h) (sdl.ttf:get-text-size font text)
    (make-v w h)))

(defun create-text-texture! (renderer font text color)
  ;; TODO: This should be a part of SDL.ttf at this point.
  (let ((surf (sdl.ttf:render-text-solid font text color)))
    (multiple-value-prog1
	(values (sdl:create-texture-from-surface renderer surf) (get-text-size font text))
      (sdl:free-surface surf))))

(defun color->hex (color)
  (let ((byte 0))
    (loop for i to 3
       do
	 (setf (ldb (byte 8 (* 8 i)) byte) (aref color i)))
    byte))

(defun get-character-texture! (char font)
  (let ((tex (gethash char *character-textures*)))
    (if tex
	tex
	(setf (gethash char *character-textures*)
	      (multiple-value-list
	       (create-text-texture! (aval *env* :renderer)
				     font
				     (string char)
				     (color->hex *white*)))))))

(defun draw-text-line (pos text)
  "Add a text-line drawing to the debug render list."
  (push-debug-render
   (make-text-line-drawing :pos pos :text text :layer :text)))

(defun render-background! (renderer camera-pos)
  (let ((len (tiles 4)))
    (dotimes (x (1+ (floor (x *window-dims*) len)))
      (dotimes (y (+ 2 (floor (y *window-dims*) len)))
	(render-sprite!
	 renderer
	 (make-sprite-drawing
	  :layer :foreground
	  :sheet-key :bk-blue
	  :src-rect (create-rect (zero-v) (both-v len))
	  :pos
	  (make-v (+ (* (1- x) len)
		     (mod (* *parallax-scale* (- (x camera-pos))) len))
		  (+ (* (1- y) len)
		     (mod (* *parallax-scale* (y camera-pos)) len))))
	 (zero-v))))))

(defun render-text! (renderer font r)
  (let ((start-pos (aval r :pos))
	(text (aval r :text)))
    (loop for c across text
       for i from 0
       do
	 (destructuring-bind (texture dims)
	     (get-character-texture! c font)
	   (sdl:render-texture
	    renderer
	    texture
	    (rect->sdl-rect (create-rect (zero-v) dims))
	    (rect->sdl-rect
	     (create-rect (+ start-pos (zero-v :x (* i (x dims))))
			  dims)))))))

(defun render-drawing! (drawing renderer font camera-pos)
  (case (aval drawing :type)
    (:sprite (render-sprite! renderer drawing camera-pos))
    (:texture (render-texture! renderer drawing camera-pos))
    (:rect (render-rect! renderer drawing camera-pos))
    (:line (render-line! renderer drawing camera-pos))
    (:text (render-text! renderer font drawing))))

(defun game-drawings (drawings)
  "Return drawings that are part of the game, given that drawings
are sorted by layer."
  (remove-if
   #_(member _ *screen-layers*)
   drawings
   :key #'drawing-layer))

(defun hud-drawings (drawings)
  "Return drawings that are part of the hud, given that drawings
are sorted by layer."
  (remove-if-not
   #_(member _ *screen-layers*)
   drawings
   :key #'drawing-layer))

(defun render! (render-list camera-pos)
  (let ((renderer (aval *env* :renderer)))
    (sdl:set-render-draw-color renderer 128 128 128 255)
    (sdl:render-clear renderer)

    (let ((drawings (nsort-by-layer (remove-invisible-layers render-list)))
	  (font (aval *env* :font)))
      (render-background! renderer camera-pos)

      (dolist (r (game-drawings drawings))
	(render-drawing! r renderer font camera-pos))
      
      (dolist (r (hud-drawings drawings))
	(render-drawing! r renderer font (zero-v))))

    (sdl:render-present renderer)))

(defun group-by (key list)
  "Group list into lists sharing the same key.
Key is a function. 
Does not modify the order of the list.
Returns a list of (key . sublist) pairs."
  (loop until (null list)
     collecting
       (let ((key-val (funcall key (car list))))
	 (cons key-val
	       (loop
		  while (and list
			     (eq (funcall key (car list)) key-val))
		  collecting (car list)
		  do (setq list (cdr list)))))))

(defun min-list (lst &key (key #'identity))
  "Returns the minimum element of a list."
  (if (null lst)
      nil
      (let ((result lst)
	    (result-val (funcall key (car lst))))
	(loop for l in (cdr lst)
	   do
	     (let ((val (funcall key l)))
	       (when (< val result-val)
		 (setq result l
		       result-val val))))
	result)))

(defun max-list (lst &key (key #'identity))
  "Returns the minimum element of a list."
  (if (null lst)
      nil
      (let ((result (car lst))
	    (result-val (funcall key (car lst))))
	(loop for l in (cdr lst)
	   do
	     (let ((val (funcall key l)))
	       (when (> val result-val)
		 (setq result l
		       result-val val))))
	result)))

(defun bounding-rect (rects)
  "Return a rect that minimally bounds all of rects."
  (let ((xpos (lambda (d) (x (aval d :pos))))
	(ypos (lambda (d) (y (aval d :pos)))))
    (let ((xmin (min-list rects :key xpos))
	  (ymin (min-list rects :key ypos))
	  (xmax (max-list rects :key xpos))
	  (ymax (max-list rects :key ypos)))
      (make-rect :pos (make-v (funcall xpos xmin)
			      (funcall ypos ymin))
		 :size
		 (make-v
		  (- (+ (funcall xpos xmax)
			(x (aval xmax :size)))
		     (funcall xpos xmin))
		  (- (+ (funcall ypos xmax)
			(y (aval ymax :size)))
		     (funcall ypos ymin)))))))

(defun drawings-rect (drawings)
  "Return a rect that minimally encompasses all drawings."
  (bounding-rect (mapcar (lambda (d)
			   (funcall (aval d :rect-fn) d))
			 drawings)))

(defun render-drawings-to-texture (layer drawings)
  "Render all drawings on a given layer to an SDL texture."
  (let* ((renderer (aval *env* :renderer))
	 (rect (drawings-rect drawings))
	 (target (claim-resource
		  :texture
		  (gensym)
		  (sdl:create-texture
		   renderer
		   (sdl:get-window-display-mode-format (aval *env* :window))
		   :target
		   (x (aval rect :size))
		   (y (aval rect :size))))))
    (sdl:set-texture-blend-mode target :blend)
    (sdl:set-render-target renderer target)
    (sdl:set-render-draw-color renderer 0 0 0 0)
    (sdl:render-clear renderer)
    (dolist (d drawings)
      (render-drawing! d
		       renderer
		       (aval *env* :font)
		       (rect-pos rect)))
    (sdl:set-render-target renderer (cffi:null-pointer))
    (make-texture-drawing :layer layer
			  :texture target
			  :src-rect (make-rect :pos (zero-v)
					       :size (rect-size rect))
			  :pos (rect-pos rect))))

(defun ncompile-drawings (drawings)
  "Return a list of compiled drawings given a list of drawings."
  (mapcar (lambda (pair)
	    (render-drawings-to-texture (car pair) (cdr pair)))
	  (group-by #'drawing-layer (nsort-by-layer drawings))))
