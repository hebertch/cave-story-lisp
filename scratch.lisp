(ql:quickload :cave-story)

(in-package :cave-story)
(swank:set-default-directory "/home/chebert/Projects/lisp/cave-story")

(main!)

(read-pxa-file "./content/stages/Cave.pxa")

(read-pxm-file "./content/stages/Cave.pxm")
(aval (read-pxm-file "./content/stages/Cave.pxm") :tile-offset-idxs)

(defun pxm-tile-offset-idx->tile-v (idx)
  "Get the tile position given an index into the pxa array."
  (make-v (mod idx 16) (floor idx 16)))

(defun pxm-and-attrs->stage (pxm attrs)
  "Create a stage from a list of row-major tiles, and a vector of
tile attribute lists."
  (let ((width (aval pxm :width))
	(height (aval pxm :height))
	(tile-offset-idxs (aval pxm :tile-offset-idxs)))
    (let ((stage (make-array (list width height))))
      (loop for x from 0 below width do
	   (loop for y from 0 below height do
		(setf (aref stage x y)
		      (list (elt attrs (car tile-offset-idxs))
			    (pxm-tile-offset-idx->tile-v
			     (car tile-offset-idxs))))
	       (setq tile-offset-idxs
		     (cdr tile-offset-idxs))))
      stage)))

(defun stage-from-file-data (pxm pxa)
  "Create a stage given the file data."
  (pxm-and-attrs->stage pxm
			(map 'vector #'tile-attribute-num->tile-attributes
			     pxa)))

(stage-from-file-data
 (read-pxm-file "./content/stages/Cave.pxm")
 (read-pxa-file "./content/stages/Cave.pxa"))

(defparameter *tile-attributes*
  '(NIL NIL (:DESTROYABLE) (:SOLID-NPC) (:SOLID-NPC) (:SOLID-PLAYER :SOLID-NPC)
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL (:FOREGROUND) (:SOLID-PLAYER :SOLID-NPC :SOLID-SHOT :FOREGROUND)
    (:HURTS-PLAYER :FOREGROUND)
    (:SOLID-PLAYER :SOLID-NPC :SOLID-SHOT :FOREGROUND :DESTROYABLE)
    (:SOLID-NPC :FOREGROUND) (:FOREGROUND) (:SOLID-PLAYER :FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND)
    (:SOLID-PLAYER :FOREGROUND) (:SOLID-PLAYER :FOREGROUND)
    (:SOLID-PLAYER :FOREGROUND) (:SOLID-PLAYER :FOREGROUND) (:FOREGROUND :SLOPE)
    (:FOREGROUND :SLOPE) (:FOREGROUND :SLOPE) (:FOREGROUND :SLOPE)
    (:FOREGROUND :SLOPE) (:FOREGROUND :SLOPE) (:FOREGROUND :SLOPE)
    (:FOREGROUND :SLOPE) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND :WATER)
    (:SOLID-PLAYER :SOLID-NPC :SOLID-SHOT :FOREGROUND)
    (:HURTS-PLAYER :FOREGROUND :WATER) (:FOREGROUND) (:SOLID-NPC :FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND)
    (:FOREGROUND) (:FOREGROUND :WATER :SLOPE) (:FOREGROUND :WATER :SLOPE)
    (:FOREGROUND :WATER :SLOPE) (:FOREGROUND :WATER :SLOPE)
    (:FOREGROUND :WATER :SLOPE) (:FOREGROUND :WATER :SLOPE)
    (:FOREGROUND :WATER :SLOPE) (:FOREGROUND :WATER :SLOPE) (:FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND) (:FOREGROUND)
    (:FOREGROUND) (:FOREGROUND) (:CURRENT) (:CURRENT) (:CURRENT) (:CURRENT) NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL (:FOREGROUND :WATER :CURRENT)
    (:FOREGROUND :WATER :CURRENT) (:FOREGROUND :WATER :CURRENT)
    (:FOREGROUND :WATER :CURRENT) NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
    NIL NIL NIL NIL))

(defun tile-attribute-num->tile-attributes (num)
  (let ((attrs (elt *tile-attributes* num)))
    (if (member :slope attrs)
	(let ((slope-idx (cond ((>= num 112)
				(- num 112))
			       (t (- num #x50)))))
	 (cons (elt '(:ltt :lts :rts :rtt :lbt :lbs :rbs :rbt)
		    slope-idx)
	       attrs))
	attrs)))

(defun read-tile-key-table ()
  "Read the tilekey.dat file to generate tile-attributes."
  (mapcar (lambda (num)
	    (remove nil (list (unless (zerop (logand num #x1)) :solid-player)
			      (unless (zerop (logand num #x2)) :solid-npc)
			      (unless (zerop (logand num #x4)) :solid-shot)
			      (unless (zerop (logand num #x10)) :hurts-player)
			      (unless (zerop (logand num #x20)) :foreground)
			      (unless (zerop (logand num #x40)) :destroyable)
			      (unless (zerop (logand num #x80)) :water)
			      (unless (zerop (logand num #x100)) :current)
			      (unless (zerop (logand num #x200)) :slope))))
	  (with-open-file (stream "~/Projects/nx/tilekey.dat"
				  :element-type '(unsigned-byte 8))
	    (loop for i from 1 to 256
	       collecting (read-uint32 stream)))))
