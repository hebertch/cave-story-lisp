(in-package :cave-story)

;; TODO: Merge?

(defparameter sound-effects (make-hash-table))
(defparameter sfx-fnames
  '(:step "Step"
    :jump "Jump"
    :head-bump "HeadBump"
    :land "Land"))

(defun put-sfx (sfx-fnames)
  "A Property list of Key=>Fname"
  (loop for (key fname) on sfx-fnames by #'cddr
     do
       (put-sound key fname)))

(defun wav-path (fname)
  (format nil "./content/sfx/~A.wav" fname))

(defun cleanup-sfx ()
  "Releases the SDL Memory. Call when done."
  (dohash (k v) sound-effects
    (unless (typep v 'string)
      (sdl.mixer:free-chunk v)))
  (clrhash sound-effects))

(defun put-sound (keysym fname)
  "Expects fname WITHOUT extension. e.g. PrtCave for PrtCave.bmp"
  (setf (gethash keysym sound-effects) fname))

(defun get-sound (keysym)
  "Gets the sound. Loads from WAV if necessary."
  (awhen (gethash keysym sound-effects)
    (if (typep it 'string)
	(setf (gethash keysym sound-effects) (sdl.mixer:load-wav (wav-path it)))
	it)))

(defparameter sfx-play-list nil)

(defstruct sound key (loops 0))

(defun play-sound (key &optional (loops 0))
  (sdl.mixer:play-channel -1 (get-sound key) loops))

(defun play-sounds (sfx-play-list)
  (dolist (s sfx-play-list)
    (play-sound (sound-key s)
		(sound-loops s))))
