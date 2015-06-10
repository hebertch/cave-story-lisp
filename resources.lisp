(in-package :cave-story)

(defstruct resource-type
  put-fn
  fnames-fn
  cleanup-fn
  load-fn)

(defparameter resource-types nil)

(defmacro def-resource-type (name (load-args &body load-forms) fnames-form destruct-fn)
  "Introduces Anaphora of FNAME into the load definition. This is to keep consistent args with
the get- function that is produced."
  (with-gensyms (hash-table rt)
    `(let (,rt
	   (,hash-table (make-hash-table)))
       (push (setf ,rt
		   (make-resource-type
		    :cleanup-fn
		    (lambda ()
		      (dohash (k v) ,hash-table
			(unless (typep v 'string)
			  (funcall ,destruct-fn v)))
		      (clrhash ,hash-table))
		    :fnames-fn
		    (lambda ()
		      ,fnames-form)
		    :put-fn
		    (lambda ()
		      (loop for (key fname) on ,fnames-form by #'cddr
			 do
			   (setf (gethash key ,hash-table) fname)))
		    :load-fn
		    ;; NOTE: FNAME Anaphora introduced here.
		    (lambda (fname ,@load-args)
		      ,@load-forms)))
	     resource-types)
       (defun ,(symbolicate 'get- name) (keysym ,@load-args)
	 (let ((ss (gethash keysym ,hash-table)))
	   (when ss
	     (if (typep ss 'string)
		 (setf (gethash keysym ,hash-table)
		       (funcall (resource-type-load-fn ,rt)
				ss
				,@load-args))
		 ss)))))))

(defun put-all-resources ()
  (dolist (rt resource-types)
    (funcall (resource-type-put-fn rt))))

(defun cleanup-all-resources ()
  (dolist (rt resource-types)
    (funcall (resource-type-cleanup-fn rt))))

;;; SPRITES
(defparameter spritesheet-fnames
  '(:my-char "MyChar"
    :npc-sym "NpcSym"
    :prt-cave "PrtCave"
    :arms "Arms"
    :bullet "Bullet"
    :npc-cemet "NpcCemet"
    :caret "Caret"
    :text-box "TextBox"))

(defun bmp-path (fname)
  (format nil "./content/~A.bmp" fname))

(defun load-spritesheet (renderer path)
  "Loads the BMP from PATH masking out black pixels."
  (let ((surf (sdl:load-bmp path)))
    (cffi:with-foreign-slots ((sdl:pixel-format) surf (:struct sdl:surface))
      (sdl:set-color-key surf 1 (sdl:map-rgb sdl:pixel-format 0 0 0)))
    (prog1
	(sdl:create-texture-from-surface renderer surf)
      (sdl:free-surface surf))))

(def-resource-type spritesheet
    ((renderer)
     (load-spritesheet renderer (bmp-path fname)))
  spritesheet-fnames
  #'sdl:destroy-texture)

;;; MUSIC
(defstruct song intro loop)
(defvar current-song)

(defparameter song-names
  '(:curly "curly"
    :fanfare1 "fanfale1"))

(defun load-song (name)
  (make-song
   :loop  (sdl.mixer:load-mus (format nil "./content/remastered-music/~A_loop.ogg" name))
   :intro (sdl.mixer:load-mus (format nil "./content/remastered-music/~A_intro.ogg" name))))

(defun destroy-song (s)
  (sdl.mixer:free-music (song-intro s))
  (sdl.mixer:free-music (song-loop s)))

(defun music-update ()
  "Call as often as possible."
  (when (and current-song (= 0 (sdl.mixer:playing-music)))
    ;; When the song intro has finished, switch to the loop portion.
    (sdl.mixer:play-music (song-loop current-song) -1)))

(defun stop-music ()
  (sdl.mixer:halt-music)
  (nilf current-song))

(defun switch-to-new-song (song-key)
  (sdl.mixer:halt-music)
  (setf current-song (get-song song-key))
  (sdl.mixer:play-music (song-intro current-song) 0))

(defun percent->volume (fixnum-percent)
  (floor (* 128 fixnum-percent) 100))

(defun set-music-volume (fixnum-percent)
  "Given an integer from 1-100 sets the volume of the music."
  (sdl.mixer:volume-music (percent->volume fixnum-percent)))

(comment-code
  ;; TODO: These still need to be fleshed out. Can't work with SDL_Mixer, because we can't store positions in songs.
  (defun pause-and-play-new-song (song-key)
    )

  (defun resume-paused-music ()
    ))

(def-resource-type song
    (()
     (load-song fname))
  song-names
  #'destroy-song)

;;; SOUNDS

(defparameter sfx-fnames
  '(:step "Step"
    :jump "Jump"
    :hurt "Hurt"
    :enemy-explode "EnemyExplode"
    :head-bump "HeadBump"
    :land "Land"
    :dissipate "Dissipate"
    :hit-wall "HitWall"
    :polar-star-shoot-3 "PolarStarShoot3"
    :dorito-bounce "DoritoBounce"
    :pickup "Pickup"
    :enemy-hurt "EnemyHurt"
    :player-die "PlayerDie"))

(defun wav-path (fname)
  (format nil "./content/sfx/~A.wav" fname))

(defun play-sounds (sfx-play-list)
  ;; This works like the renderer. Merge?
  (dolist (s sfx-play-list)
    (sdl.mixer:play-channel -1 (get-sound s) 0)))

(defparameter sfx-play-list nil)

(defun push-sound (key-sym)
  (push key-sym sfx-play-list))

(def-resource-type sound
    (()
     (sdl.mixer:load-wav (wav-path fname)))
  sfx-fnames
  #'sdl.mixer:free-chunk)
