(in-package :cave-story)

(defparameter spritesheets (make-hash-table))

(defparameter spritesheet-fnames
  '(:my-char "MyChar"
    :prt-cave "PrtCave"
    :arms "Arms"
    :bullet "Bullet"
    :npc-cemet "NpcCemet"))

(defun put-spritesheets (spritesheet-fnames)
  "A Property list of Key=>Fname"
  (loop for (key fname) on spritesheet-fnames by #'cddr
     do
       (put-spritesheet key fname)))

(defun load-spritesheet (renderer path)
  "Loads the BMP from PATH masking out black pixels."
  (let ((surf (sdl:load-bmp path)))
    (cffi:with-foreign-slots ((sdl:pixel-format) surf (:struct sdl:surface))
      (sdl:set-color-key surf 1 (sdl:map-rgb sdl:pixel-format 0 0 0)))
    (prog1
	(sdl:create-texture-from-surface renderer surf)
      (sdl:free-surface surf))))

(defun bmp-path (fname)
  (format nil "./content/~A.bmp" fname))

(defun cleanup-spritesheets ()
  "Releases the SDL Memory. Call when done."
  (dohash (k v) spritesheets
    (unless (typep v 'string)
      (sdl:destroy-texture v)))
  (clrhash spritesheets))

(defun put-spritesheet (keysym fname)
  "Expects fname WITHOUT extension. e.g. PrtCave for PrtCave.bmp"
  (setf (gethash keysym spritesheets) fname))

(defun get-spritesheet (keysym renderer)
  "Gets the spritesheet. Loads from BMP if necessary."
  (let ((ss (gethash keysym spritesheets)))
    (when ss
      (if (typep ss 'string)
	  (setf (gethash keysym spritesheets) (load-spritesheet renderer (bmp-path ss)))
	  ss))))

