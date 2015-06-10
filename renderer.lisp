(in-package :cave-story)

(defstruct sprite-drawing
  layer
  sheet-key
  src-rect
  pos)

(defstruct rect-drawing
  (layer :debug)
  color
  rect
  filled?)

(defstruct line-drawing
  (layer :debug)
  color a b)

(defparameter red #(255 0 0 255))
(defparameter green #(0 255 0 255))
(defparameter blue #(0 0 255 255))
(defparameter white #(255 255 255 255))
(defparameter yellow #(255 255 0 255))
(defparameter magenta #(255 0 255 255))
(defparameter cyan #(0 255 255 255))

(defgeneric drawing-layer (d))
(defmethod drawing-layer ((d sprite-drawing))
  (sprite-drawing-layer d))
(defmethod drawing-layer ((d rect-drawing))
  (rect-drawing-layer d))
(defmethod drawing-layer ((d line-drawing))
  (line-drawing-layer d))

(defvar render-debug? t
  "Whether to render the DEBUG-RENDER-LIST")
(defvar render-list nil
  "The list of drawings to be rendered once per frame.")
(defparameter screen-render-list nil
  "The list of drawings to be rendered once per frame.
These are drawn relative to the SCREEN and not relative to the camera position.")

(defun set-draw-color (renderer c)
  "Sets the sdl:renderer draw color. c: #(r g b a)"
  (sdl:set-render-draw-color renderer (aref c 0) (aref c 1) (aref c 2) (aref c 3)))

(defun rect->sdl-rect (r)
  "Transforms a CAVE-STORY:RECT into an SDL:RECT"
  (sdl:make-rect (round (x (rect-pos r)))
		 (round (y (rect-pos r)))
		 (round (x (rect-size r)))
		 (round (y (rect-size r)))))

(defun render-sprite (renderer sprite-drawing camera-pos)
  "Renderer func. Renders a sprite."
  (let ((src (sprite-drawing-src-rect sprite-drawing))
	(pos (sub-v (sprite-drawing-pos sprite-drawing) camera-pos))
	(keysym (sprite-drawing-sheet-key sprite-drawing)))
    (let ((src-pos (rect-pos src))
	  (size (rect-size src)))
      (sdl:render-texture renderer (get-spritesheet keysym renderer)
			  (sdl:make-rect (x src-pos) (y src-pos) (x size) (y size))
			  (sdl:make-rect (round (x pos)) (round (y pos)) (x size) (y size))))))

(defun render-rect (renderer rect-drawing camera-pos)
  "Renderer func. Renders a rect."
  (set-draw-color renderer (rect-drawing-color rect-drawing))
  (sdl:render-rect renderer (rect->sdl-rect (rect-offset (rect-drawing-rect rect-drawing)
							 (sub-v (zero-v) camera-pos)))
		   :filled? (rect-drawing-filled? rect-drawing)))

(defun render-line (renderer ld camera-pos)
  "Renderer func. Renders a line."
  (set-draw-color renderer (line-drawing-color ld))
  (let ((a (sub-v (line-drawing-a ld) camera-pos))
	(b (sub-v (line-drawing-a ld) camera-pos)))
    (sdl:render-draw-line renderer
			  (round (x a))
			  (round (y a))
			  (round (x b))
			  (round (y b)))))

(defun push-render (r)
  "Interface to the RENDER-LIST"
  (push r render-list))

(defun push-screen-render (r)
  "Interface to the SCREEN-RENDER-LIST"
  (push r screen-render-list))

(defun draw-slope (tile-pos tile-type)
  "Pushes a slope to the DEBUG-RENDER-LIST"
  (let ((pos (tile-pos->pos tile-pos)))
    (push-render (make-line-drawing
		  :color white
		  :a (make-v (x pos) (tile-slope-pos-y
				      tile-pos
				      tile-type
				      (x pos)))
		  :b (make-v (+ (x pos) tile-size) (tile-slope-pos-y
						    tile-pos
						    tile-type
						    (+ (x pos) tile-size)))))))

(defun pixel-v (v)
  (make-v (round (x v))
	  (round (y v))))

(defparameter debug-layers
  (list :debug :debug-damage-collision :debug-stage-collision :debug-pickup :debug-damageable :debug-dynamic-collision))
(defparameter game-layers (list :gun :enemy :pickup :player :projectile :foreground :particle :hud-bg :hud))
(defparameter layers (append game-layers debug-layers))
(defparameter visible-layers game-layers)

(defun remove-visible-layer (layer)
  (setf visible-layers (set-difference visible-layers (list layer))))

(defun add-visible-layer (layer)
  (pushnew layer visible-layers))

(defun toggle-visible-layer (layer)
  (if (member layer visible-layers)
      (remove-visible-layer layer)
      (add-visible-layer layer)))

(defun layer< (a b)
  (< (position a layers) (position b layers)))

(defun sort-by-layers (render-list)
  (sort
   (remove-if-not
    (lambda (d)
      (member (drawing-layer d) visible-layers))
    render-list)
   #'layer< :key #'drawing-layer))

(defun draw-rect (rect color &key (layer :debug) filled?)
  (push-render
   (make-rect-drawing :color color
		      :rect rect
		      :layer layer
		      :filled? filled?)))

(defun draw-tile-rect (pos color &key (layer :debug) filled?)
  (draw-rect (tile-rect pos) color :layer layer :filled? filled?))

(defun draw-point (pos color &key (layer :debug) (size 5))
  (draw-rect (centered-rect pos (both-v size))
	     color
	     :layer layer
	     :filled? t))

(defun draw-line (a b color &key (layer :debug))
  (push-render
   (make-line-drawing :a a :b b :layer layer :color color)))

(defun draw-sprite (layer sheet-key src-rect pos)
  (push-render (make-sprite-drawing :layer layer
				    :sheet-key sheet-key
				    :src-rect src-rect
				    :pos pos)))

(defun draw-hud-sprite (layer sheet-key src-rect pos)
  (push-screen-render (make-sprite-drawing :layer layer
					   :sheet-key sheet-key
					   :src-rect src-rect
					   :pos pos)))

(defun render (renderer render-list camera-pos)
  (sdl:set-render-draw-color renderer 128 128 128 255)
  (sdl:render-clear renderer)

  (setf render-list (sort-by-layers render-list))
  (dolist (r render-list)
    (cond
      ((sprite-drawing-p r) (render-sprite renderer r camera-pos))
      ((rect-drawing-p r) (render-rect renderer r camera-pos))
      ((line-drawing-p r) (render-line renderer r camera-pos))))

  (setf screen-render-list (sort-by-layers screen-render-list))

  (dolist (r screen-render-list)
    (cond
      ((sprite-drawing-p r) (render-sprite renderer r (zero-v)))
      ((rect-drawing-p r) (render-rect renderer r (zero-v)))
      ((line-drawing-p r) (render-line renderer r (zero-v)))))

  (sdl:render-present renderer))
