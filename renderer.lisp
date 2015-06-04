(in-package :cave-story)

(defstruct sprite-drawing
  layer
  sheet-key
  src-rect
  pos)

(defstruct rect-drawing
  color
  rect
  filled?)

(defstruct line-drawing
  color a b)

(defvar render-debug? t
  "Whether to render the DEBUG-RENDER-LIST")
(defvar render-list nil
  "The list of drawings to be rendered once per frame.")
(defparameter debug-render-list nil
  "The list of drawings to be rendered once per frame.
These are rendered on top of the render-list, and are toggled with RENDER-DEBUG?")

(defun set-draw-color (renderer c)
  "Sets the sdl:renderer draw color. c: #(r g b a)"
  (sdl:set-render-draw-color renderer (aref c 0) (aref c 1) (aref c 2) (aref c 3)))

(defun rect->sdl-rect (r)
  "Transforms a CAVE-STORY:RECT into an SDL:RECT"
  (sdl:make-rect (round (x (rect-pos r)))
		 (round (y (rect-pos r)))
		 (round (x (rect-size r)))
		 (round (y (rect-size r)))))

(defun render-sprite (renderer sprite-drawing)
  "Renderer func. Renders a sprite."
  (let ((src (sprite-drawing-src-rect sprite-drawing))
	(pos (sprite-drawing-pos sprite-drawing))
	(keysym (sprite-drawing-sheet-key sprite-drawing)))
    (let ((src-pos (rect-pos src))
	  (size (rect-size src)))
      (sdl:render-texture renderer (get-spritesheet keysym renderer)
			  (sdl:make-rect (x src-pos) (y src-pos) (x size) (y size))
			  (sdl:make-rect (round (x pos)) (round (y pos)) (x size) (y size))))))

(defun render-rect (renderer rect-drawing)
  "Renderer func. Renders a rect."
  (set-draw-color renderer (rect-drawing-color rect-drawing))
  (sdl:render-rect renderer (rect->sdl-rect (rect-drawing-rect rect-drawing))
		   :filled? (rect-drawing-filled? rect-drawing)))

(defun render-line (renderer ld)
  "Renderer func. Renders a line."
  (set-draw-color renderer (line-drawing-color ld))
  (sdl:render-draw-line renderer
			(round (x (line-drawing-a ld)))
			(round (y (line-drawing-a ld)))
			(round (x (line-drawing-b ld)))
			(round (y (line-drawing-b ld)))))

(defun push-debug-render (r)
  "Interface to the DEBUG-RENDER-LIST"
  (push r debug-render-list))
(defun push-render (r)
  "Interface to the RENDER-LIST"
  (push r render-list))

(defun draw-slope (tile-pos tile-type)
  "Pushes a slope to the DEBUG-RENDER-LIST"
  (let ((pos (tile-pos->pos tile-pos)))
    (push-debug-render (make-line-drawing
			:color #(255 255 255 255)
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

(defparameter layers '(:gun :enemy :player :projectile :foreground))

(defun layer< (a b)
  (< (position a layers) (position b layers)))

(defun render (renderer render-list)
  (sdl:set-render-draw-color renderer 128 128 128 255)
  (sdl:render-clear renderer)

  (setf render-list (sort render-list #'layer< :key #'sprite-drawing-layer))

  (dolist (r render-list)
    (assert (sprite-drawing-p r))
    (render-sprite renderer r))

  (when render-debug?
    (dolist (r debug-render-list)
      (cond
	((sprite-drawing-p r) (render-sprite renderer r))
	((rect-drawing-p r) (render-rect renderer r))
	((line-drawing-p r) (render-line renderer r)))))

  (sdl:render-present renderer))
