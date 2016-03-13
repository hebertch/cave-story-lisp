(in-package :cave-story)

(defvar* *song-names-table*
    #(nil
      (:song-egg "wanpaku")
      (:song-safety "anzen")
      (:song-game-over "gameover")
      (:song-gravity "gravity")
      (:song-grasstown "weed")
      (:song-meltdown-2 "mdown2")
      (:song-eyes-of-flame "fireeye")
      (:song-gestation "vivi")
      (:song-town "mura")
      (:song-fanfare-1 "fanfale1")
      (:song-balrog "ginsuke")
      (:song-cemetary "cemetery")
      (:song-plant "plant")
      (:song-pulse "kodou")
      (:song-fanfare-2 "fanfale2")
      (:song-fanfare-3 "fanfale3")
      (:song-tyrant "dr")
      (:song-escape "escape")
      (:song-jenka-1 "jenka")
      (:song-labyrinth "maze")
      (:song-access "access")
      (:song-oppression "ironh")
      ;; NOTE: Still not sure about :geothermal, but
      ;; "grand" and "kaze" are the only two left, and "grand" seems closer.
      (:song-geothermal "grand")
      (:song-theme "curly")
      (:song-oside "oside")
      (:song-hero-end "requiem")
      (:song-scorching-back "wanpak2")
      (:song-quiet "quiet")
      (:song-last-cave "lastcave")
      (:song-balcony "balcony")
      (:song-charge "lastbtl" :loop-only)
      (:song-last-battle "lastbt3")
      (:song-credits "credits")
      (:song-zombie "zonbie")
      (:song-breakdown "breakdown")
      (:song-hell "hell")
      (:song-jenka-2 "jenka2")
      (:song-waterway "marine")
      (:song-seal "ballos")
      (:song-toroko "toroko")
      (:song-white "white")
      ;; NOTE: :AZARASHI doesn't seem to be a song in cave story.
      ;; Just guessing it is "kaze" because?
      (:song-azarashi "kaze")
      nil)
  "Ordered vector of (song-key song-fname :loop-only?); accessed by tsc scripts.")

(defvar* *sound-effects-table*
    '((:snd-menu-move 1)
      (:snd-msg 2)
      (:snd-bonk-head 3)
      (:snd-switch-weapon 4)
      (:snd-menu-prompt 5)
      (:snd-hoppy-jump 6)
      (:snd-door 11)
      (:snd-block-destroy 12)
      (:snd-get-xp 14)
      (:snd-player-jump 15)
      (:snd-player-hurt 16)
      (:snd-player-die 17)
      (:snd-menu-select 18)
      (:snd-health-refill 20)
      (:snd-bubble 21)
      (:snd-chest-open 22)
      (:snd-thud 23)
      (:snd-player-walk 24)
      (:snd-funny-explode 25)
      (:snd-quake 26)
      (:snd-level-up 27)
      (:snd-shot-hit 28)
      (:snd-teleport 29)
      (:snd-enemy-jump 30)
      (:snd-tink 31)
      (:snd-polar-star-l1-2 32)
      (:snd-snake-fire 33)
      (:snd-fireball 34)
      (:snd-explosion1 35)
      (:snd-gun-click 37)
      (:snd-get-item 38)
      (:snd-em-fire 39)
      (:snd-stream1 40)
      (:snd-stream2 41)
      (:snd-get-missile 42)
      (:snd-computer-beep 43)
      (:snd-missile-hit 44)
      (:snd-xp-bounce 45)
      (:snd-ironh-shot-fly 46)
      (:snd-explosion2 47)
      (:snd-bubbler-fire 48)
      (:snd-polar-star-l3 49)
      (:snd-enemy-squeak 50)
      (:snd-enemy-hurt 51)
      (:snd-enemy-hurt-big 52)
      (:snd-enemy-hurt-small 53)
      (:snd-enemy-hurt-cool 54)
      (:snd-enemy-squeak2 55)
      (:snd-splash 56)
      (:snd-enemy-damage 57)
      (:snd-propellor 58)
      (:snd-spur-charge-1 59)
      (:snd-spur-charge-2 60)
      (:snd-spur-charge-3 61)
      (:snd-spur-fire-1 62)
      (:snd-spur-fire-2 63)
      (:snd-spur-fire-3 64)
      (:snd-spur-maxed 65)
      (:snd-expl-small 70)
      (:snd-little-crash 71)
      (:snd-big-crash 72)
      (:snd-bubbler-launch 100)
      (:snd-lightning-strike 101)
      (:snd-jaws 102)
      (:snd-charge-gun 103)
      (:snd-104 104)
      (:snd-puppy-bark 105)
      (:snd-slash 106)
      (:snd-block-move 107)
      (:snd-igor-jump 108)
      (:snd-critter-fly 109)
      (:snd-droll-shot-fly 110)
      (:snd-motor-run 111)
      (:snd-motor-skip 112)
      (:snd-booster 113)
      (:snd-core-hurt 114)
      (:snd-core-thrust 115)
      (:snd-core-charge 116)
      (:snd-nemesis-fire 117)
      (:snd-150 150)
      (:snd-151 151)
      (:snd-152 152)
      (:snd-153 153)
      (:snd-154 154)
      (:snd-155 155))
  "AList of (SOUND-KEY SOUND-IDX).")

(defun sound-effect-idx->sound-effect-key (idx)
  (first (member idx *sound-effects-table* :key #'second)))

(defstruct resource-type
  put-fn
  cleanup-fn
  load-fn)

(defvar* *resource-types* nil)

(defvar* *sfx-fnames*
    (mapcar (lambda (a)
	      (let ((key (first a))
		    (num (second a)))
		(cons key (format nil "~(fx~2,'0X~)" num))))
	    *sound-effects-table*))

(defvar* *song-names*
    (alist :ACCESS "access"
	   :ANZEN "anzen"
	   :BALCONY "balcony"
	   :BALLOS "ballos"
	   :BDOWN "bdown"
	   :BREAKDOWN "breakdown"
	   :CEMETERY "cemetery"
	   :CREDITS "credits"
	   :CURLY "curly"
	   :DR "dr"
	   :ENDING "ending"
	   :ESCAPE "escape"
	   :FANFALE1 "fanfale1"
	   :FANFALE2 "fanfale2"
	   :FANFALE3 "fanfale3"
	   :FIREEYE "fireeye"
	   :GAMEOVER "gameover"
	   :GINSUKE "ginsuke"
	   :GRAND "grand"
	   :GRAVITY "gravity"
	   :HELL "hell"
	   :IRONH "ironh"
	   :JENKA2 "jenka2"
	   :JENKA "jenka"
	   :KAZE "kaze"
	   :KODOU "kodou"
	   :LASTBT3 "lastbt3"
	   :LASTCAVE2 "lastcave2"
	   :LASTCAVE "lastcave"
	   :MARINE "marine"
	   :MAZE "maze"
	   :MDOWN2 "mdown2"
	   :MURA "mura"
	   :OSIDE "oside"
	   :PLANT "plant"
	   :QUIET "quiet"
	   :REQUIEM "requiem"
	   :SILENCE "silence"
	   :TOROKO "toroko"
	   :VIVI "vivi"
	   :WANPAK2 "wanpak2"
	   :WANPAKU_ENDING "wanpaku_ending"
	   :WANPAKU "wanpaku"
	   :WEED "weed"
	   :WHITE "white"
	   :ZONBIE "zonbie"))

(defvar* *spritesheet-fnames*
    (append (alist :my-char "MyChar"
		   :npc-sym "NpcSym"
		   :arms "Arms"
		   :bullet "Bullet"
		   :npc-cemet "NpcCemet"
		   :caret "Caret"
		   :text-box "TextBox"
		   :bk-blue "bkBlue"
		   :npc-regu "NpcRegu"
		   :npc-eggs1 "NpcEggs1")
	    (loop for d in (directory #p "./content/Prt*.bmp")
	       collecting
		 (let* ((fname (file-namestring d))
			(name  (subseq fname 3 (position #\. fname))))
		   (cons (make-keyword (string-upcase name))
			 (format nil "Prt~A" name))))))

(defvar* *stage-fields*
    '((:stage-weed :weed "Weed")
      (:stage-cent :cent "Weed")
      (:stage-santa :santa "Santa")
      (:stage-cave :cave "Cave")
      (:stage-eggs :eggs "Eggs")
      (:stage-sand-e :sand "Sand" "SandE")
      (:stage-pens-1 :pens "Pens" "Pens1")
      (:stage-hell-1 :hell "Hell" "Hell1")
      (:stage-jail-1 :jail "Jail" "Jail1")
      (:stage-maze-i :maze "Maze" "MazeI"))
  "Fields used to generate the *stage-fnames-table*.
List of (keyword sprite-key attributes-fname entities/stage-fname).")

(defvar* *stage-fnames-table*
    (mapcar (lambda (fields)
	      (let ((key (first fields))
		    (spritesheet-key (second fields))
		    (attributes-fname (third fields))
		    (fname (if (fourth fields)
			       (fourth fields)
			       (third fields))))
		(cons key
		      (alist :stage fname
			     :entities fname
			     :attributes attributes-fname
			     :spritesheet spritesheet-key))))
	    *stage-fields*)
  "Alist of (keyword . (alist stage attributes entities spritesheet)).")

;; TODO: Remove me!
(defmacro def-resource-type
    (name (load-args &body load-forms) fnames-form destruct-fn)
  "Introduces Anaphora of FNAME into the load definition. This is to keep consistent args with
the get- function that is produced."
  (with-gensyms (hash-table rt)
    `(let (,rt
	   (,hash-table (make-hash-table)))
       (push (setf ,rt
		   (make-resource-type
		    :cleanup-fn
		    (lambda ()
		      (loop for k being the hash-key in ,hash-table
			 using (hash-value v) do
			   (unless (typep v 'string)
			     (funcall ,destruct-fn v)))
		      (clrhash ,hash-table))
		    :put-fn
		    (lambda ()
		      (loop for (key . fname) in ,fnames-form
			 do
			   (setf (gethash key ,hash-table) fname)))
		    :load-fn
		    ;; NOTE: FNAME Anaphora introduced here.
		    (lambda (fname ,@load-args)
		      ,@load-forms)))
	     *resource-types*)
       (defun ,(symbolicate 'get- name) (keysym ,@load-args)
	 (let ((ss (gethash keysym ,hash-table)))
	   (when ss
	     (if (typep ss 'string)
		 (setf (gethash keysym ,hash-table)
		       (funcall (resource-type-load-fn ,rt)
				ss
				,@load-args))
		 ss)))))))

(defun put-all-resources! ()
  (dolist (rt *resource-types*)
    (funcall (resource-type-put-fn rt))))

(defun cleanup-all-resources! ()
  (dolist (rt *resource-types*)
    (funcall (resource-type-cleanup-fn rt))))

;;; SPRITES

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
  *spritesheet-fnames*
  #'sdl:destroy-texture)

;;; MUSIC
(defstruct song intro loop name)

(defun load-song (name)
  (make-song
   :name name
   :loop  (sdl.mixer:load-mus (format nil "./content/remastered-music/~A_loop.ogg" name))
   :intro (sdl.mixer:load-mus (format nil "./content/remastered-music/~A_intro.ogg" name))))

(defun destroy-song! (s)
  (sdl.mixer:free-music (song-intro s))
  (sdl.mixer:free-music (song-loop s))
  :done)

(defun music-update! ()
  "Call as often as possible."
  (let ((song (aval *env* :current-song)))
    (when (and song (= 0 (sdl.mixer:playing-music)))
      ;; When the song intro has finished, switch to the loop portion.
      (sdl.mixer:play-music (song-loop song) -1)))
  :done)

(defun stop-music! ()
  (sdl.mixer:halt-music)
  :done)

(defun switch-to-new-song! (song-key)
  (stop-music!)
  (let ((song (get-song song-key)))
    (sdl.mixer:play-music (song-intro song) 0)
    (update-env! (aset *env* :current-song song)))
  :done)

(defun percent->volume (fixnum-percent)
  (floor (* 128 fixnum-percent) 100))

(defun set-music-volume! (fixnum-percent)
  "Given an integer from 1-100 sets the volume of the music."
  (sdl.mixer:volume-music (percent->volume fixnum-percent))
  :done)

#+nil
(progn
  ;; TODO: These still need to be fleshed out. Can't work with SDL_Mixer, because we can't store positions in songs.
  (defun pause-and-play-new-song (song-key)
    )

  (defun resume-paused-music ()
    ))

(def-resource-type song
    (()
     (load-song fname))
  *song-names*
  #'destroy-song!)

;;; SOUNDS


(defun wav-path (fname)
  (format nil "./content/sfx/~A.wav" fname))

(defun play-sounds! (sfx-play-list)
  ;; This works like the renderer. Merge?
  (dolist (s sfx-play-list)
    (sdl.mixer:play-channel -1 (get-sound s) 0))
  :done)

(def-resource-type sound
    (()
     (sdl.mixer:load-wav (wav-path fname)))
  *sfx-fnames*
  #'sdl.mixer:free-chunk)

;; TODO: Generate meaningful names or just live with these?
(defun generate-song-fnames ()
  (loop for f in (directory "./content/remastered-music/*_intro.ogg")
     appending (let* ((str (file-namestring f))
		      (name (subseq str 0 (- (length str) (length "_intro.ogg")))))
		 (list (alexandria:make-keyword (format nil "~:@(~A~)" name)) name))))
