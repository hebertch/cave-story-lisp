(in-package :cave-story)

(defstruct drawing
  (layer :debug))

(defstruct (sprite-drawing (:include drawing))
  sheet-key
  src-rect
  pos)

(defstruct (rect-drawing (:include drawing))
  color
  rect
  filled?)

(defstruct (line-drawing (:include drawing))
  color a b)

(defstruct (text-line-drawing (:include drawing))
  pos text)

(defstruct (compiled-drawing (:include drawing))
  drawings)

(defparameter *red* #(255 0 0 255))
(defparameter *green* #(0 255 0 255))
(defparameter *blue* #(0 0 255 255))
(defparameter *white* #(255 255 255 255))
(defparameter *black* #(0 0 0 255))
(defparameter *yellow* #(255 255 0 255))
(defparameter *magenta* #(255 0 255 255))
(defparameter *cyan* #(0 255 255 255))

(defvar *render-debug?* t
  "Whether to render the DEBUG-RENDER-LIST")
(defvar *render-list* nil
  "The list of drawings to be rendered once per frame.")
(defparameter *debug-layers*
  (list :debug
	:debug-damage-collision
	:debug-stage-collision
	:debug-pickup
	:debug-damageable
	:debug-dynamic-collision
	:debug-camera
	:debug-mouse))
(defparameter *game-layers*
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
(defparameter *hud-layers*
  (list :hud-bg :hud :hud-fg
	:text-box :text))
(defparameter *debug-screen-layers*
  (list :debug-mouse))
(defparameter *layers* (append *game-layers* *debug-layers*
			       *hud-layers* *debug-screen-layers*))
(defparameter *screen-layers* (member (first *hud-layers*) *layers*))
(defparameter *visible-layers* (append *game-layers* *hud-layers*))
(defparameter *parallax-scale* 1/8)
(defparameter *character-textures* (make-hash-table))

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

(defun render-sprite! (renderer sprite-drawing camera-pos)
  "Renderer func. Renders a sprite."
  (let ((src (sprite-drawing-src-rect sprite-drawing))
	(pos (sub-v (sprite-drawing-pos sprite-drawing) camera-pos))
	(keysym (sprite-drawing-sheet-key sprite-drawing)))
    (let ((src-pos (rect-pos src))
	  (size (rect-size src)))
      (when (and (< (- (x size)) (x pos) (x *window-dims*))
		 (< (- (y size)) (y pos) (y *window-dims*)))
	(sdl:render-texture
	 renderer
	 (get-spritesheet keysym renderer)
	 (sdl:make-rect (x src-pos) (y src-pos) (x size) (y size))
	 (sdl:make-rect (round (x pos)) (round (y pos)) (x size) (y size)))))))


(defun render-rect! (renderer rect-drawing camera-pos)
  "Renderer func. Renders a rect."
  (set-draw-color! renderer (rect-drawing-color rect-drawing))
  (let* ((rect (rect-drawing-rect rect-drawing))
	 (pos (-v (rect-pos rect) camera-pos))
	 (size (rect-size rect)))
    (when (and (< (- (x size)) (x pos) (x *window-dims*))
	       (< (- (y size)) (y pos) (y *window-dims*)))
      (sdl:render-rect renderer
		       (rect->sdl-rect
			(rect-offset rect (sub-v (zero-v) camera-pos)))
		       :filled? (rect-drawing-filled? rect-drawing)))))

(defun render-line! (renderer ld camera-pos)
  "Renderer func. Renders a line."
  (set-draw-color! renderer (line-drawing-color ld))
  (let ((a (sub-v (line-drawing-a ld) camera-pos))
	(b (sub-v (line-drawing-b ld) camera-pos)))
    (sdl:render-draw-line renderer
			  (round (x a))
			  (round (y a))
			  (round (x b))
			  (round (y b)))))

(defun push-render! (r)
  "Interface to the *RENDER-LIST*"
  (push r *render-list*))

(defun slope-drawing (tile-pos tile-type color layer)
  (let* ((pos (tile-pos->pos tile-pos))
	 (left (x pos))
	 (right (+ left *tile-size*)))
    (make-line-drawing
     :layer layer
     :color color
     :a (make-v left (tile-slope-pos-y tile-pos tile-type left))
     :b (make-v right (tile-slope-pos-y tile-pos tile-type right)))))

(defun draw-slope! (tile-pos tile-type &key (layer :debug) (color *white*))
  "Pushes a slope to the DEBUG-RENDER-LIST"
  (push-render! (slope-drawing tile-pos tile-type color layer)))

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

(defun draw-rect! (rect color &key (layer :debug) filled?)
  (push-render!
   (make-rect-drawing :color color
		      :rect rect
		      :layer layer
		      :filled? filled?)))

(defun tile-rect-drawing (pos color layer filled?)
  (make-rect-drawing :color color
		     :rect (tile-rect pos)
		     :layer layer
		     :filled? filled?))

(defun draw-tile-rect! (pos color &key (layer :debug) filled?)
  (push-render! (tile-rect-drawing pos color layer filled?)))

(defun point-drawing (pos color layer size)
  (make-rect-drawing :rect (centered-rect pos (both-v size))
		     :color color
		     :layer layer
		     :filled? t))

(defun draw-point! (pos color &key (layer :debug) (size 5))
  (push-render! (point-drawing pos color layer size)))

(defun draw-line! (a b color &key (layer :debug))
  (push-render!
   (make-line-drawing :a a :b b :layer layer :color color)))

(defun draw-sprite! (layer sheet-key src-rect pos)
  (push-render! (make-sprite-drawing :layer layer
				     :sheet-key sheet-key
				     :src-rect src-rect
				     :pos pos)))

(defun draw-hud-sprite! (layer sheet-key src-rect pos)
  (push-render! (make-sprite-drawing :layer layer
				     :sheet-key sheet-key
				     :src-rect src-rect
				     :pos pos)))

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
	      (multiple-value-list (create-text-texture! *renderer*
							 font
							 (string char)
							 (color->hex *white*)))))))

(defun draw-text-line! (pos text)
  (push-render!
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
  (let ((start-pos (text-line-drawing-pos r))
	(text (text-line-drawing-text r)))
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
	     (create-rect (+v start-pos (zero-v :x (* i (x dims))))
			  dims)))))))

(defun render-drawing! (drawing renderer font camera-pos)
  (cond
    ((compiled-drawing-p drawing)
     (mapc #_(render-drawing! _ renderer font camera-pos)
	   (compiled-drawing-drawings drawing)))
    ((sprite-drawing-p drawing) (render-sprite! renderer drawing camera-pos))
    ((rect-drawing-p drawing) (render-rect! renderer drawing camera-pos))
    ((line-drawing-p drawing) (render-line! renderer drawing camera-pos))
    ((text-line-drawing-p drawing) (render-text! renderer font drawing))))

(defun render! (render-list camera-pos)
  (sdl:set-render-draw-color *renderer* 128 128 128 255)
  (sdl:render-clear *renderer*)

  (setq render-list (nsort-by-layer (remove-invisible-layers render-list)))

  (let ((render-list
	 (remove-if
	  (lambda (x) (member (drawing-layer x) *screen-layers*))
	  render-list))
	(screen-render-list
	 (remove-if
	  (lambda (x) (not (member (drawing-layer x) *screen-layers*)))
	  render-list)))

    (render-background! *renderer* camera-pos)
    
    (dolist (r render-list)
      (render-drawing! r *renderer* *font* camera-pos))

    (dolist (r screen-render-list)
      (render-drawing! r *renderer* *font* (zero-v))))

  (sdl:render-present *renderer*))

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

(defun ncompile-drawings (drawings)
  "Return a list of compiled drawings given a list of drawings."
  (mapcar (lambda (pair)
	    (let ((layer (car pair))
		  (drawings (cdr pair)))
	      (make-compiled-drawing :layer layer :drawings drawings)))
	  (group-by #'drawing-layer (nsort-by-layer drawings))))