(ql:quickload :cave-story)

(in-package :cave-story)
(swank:set-default-directory "/home/chebert/Projects/lisp/cave-story")


(read-pxa-file "./content/stages/Cave.pxa")

(read-pxm-file "./content/stages/Cave.pxm")
(aval (read-pxm-file "./content/stages/Cave.pxm") :tile-offset-idxs)

(stage-from-file-data
 (read-pxm-file "./content/stages/Cave.pxm")
 (read-pxa-file "./content/stages/Cave.pxa"))


;; Rolling Average
;; every time the function is run, add it to the buffer
;; average sum = the times in buffer / number of items in buffer
;; percentage of frame-time = frame-time / avg * 100

(defparameter *rolling-avg* (alist :num-entries 360))

(rolling-average-time *rolling-avg* (sleep 1))

(drawings-rect (aval (first (ncompile-drawings (stage-drawing (cave-stage)))) :drawings))

(read-pxe-file "./content/stages/Cave.pxe")

;; Cave flags
'(:APPEAR-ON-FLAG-ID
  :FACES-RIGHT
  :DISAPPEAR-ON-FLAG-ID
  :SCRIPT-ON-ACTIVATE)

;; Cave Types
'(:LIFE-CAPSULE
  :DROPLET-SPAWNER
  :SPIKE-SMALL
  :BAT-BLUE
  :CRITTER-HOPPING-BLUE
  :DOOR-ENEMY
  :DOOR
  :SAVE-SIGN)

(defparameter *tsc* (decrypt-tsc-file "./content/stages/Sand.tsc"))
(parse-decrypted-tsc-script *tsc*)

(defun parse-decrypted-tsc-script (script)
  "Given the decrypted tsc script, return a parsed result."
  (mapcar
   (lambda (d)
     (list (car d) (cons :script (cdr d))))
   (split-sequence:split-sequence
    '(:end)
    (mapcan #'parse-tsc-line
	    (remove-if (lambda (d) (zerop (length d)))
		       (split-sequence:split-sequence #\newline script)))
    :test #'equal
    :remove-empty-subseqs t)))

(defun rest-of-line (line start)
  (when (>= (length line) start)
    (subseq line start)))

(defun parse-tsc-line (line)
  (if (zerop (length line))
      nil
      (case (aref line 0)
	(#\# (list (cons :id (car (parse-tsc-num (subseq line 1))))))
	(#\< (let ((parse (parse-tsc-command line))) 
	       (cons (car parse) (parse-tsc-line (cdr parse)))))
	(t (let ((parse (parse-tsc-text line)))
	     (cons (car parse) (parse-tsc-line (cdr parse))))))))

(defun parse-tsc-text (line)
  "Assumes line starts with text for a message.
Parses up to the next <."
  (let ((text (first (split-sequence:split-sequence #\< line))))
    (cons text (rest-of-line line (length text)))))

(defun parse-tsc-command (line)
  "Assumes line starts with a tsc command <XYX0001:1234...
Returns (command . remaining-line)"
  (let* ((cmd (make-keyword (subseq line 1 4)))
	 (desc (second (assoc cmd *tsc-command-table*))))
    (if (and (> (length line) 4) (digit-char-p (aref line 4)))
	(let ((parse (parse-tsc-command-args (subseq line 4))))
	  (cons (list cmd
		      (cons :description desc)
		      (cons :args (car parse)))
		(cdr parse)))
	(cons (list cmd (cons :description desc))
	      (rest-of-line line 4)))))

(defun parse-tsc-command-args (line)
  "Assumes line is 0000:1234:5678..."
  (let* ((parse (parse-tsc-num line))
	 (arg (car parse))
	 (line2 (cdr parse)))
    (if (zerop (length line2))
	(cons (list arg) line2)
	(case (aref line2 0)
	  (#\: (let ((parse2 (parse-tsc-command-args (subseq line2 1))))
		 (cons (cons arg (car parse2))
		       (cdr parse2))))
	  (t (cons (list arg) line2))))))

(defun parse-tsc-num (line)
  "Assumes line starts with a tsc num 0000...
Returns (num . remaining-line)."
  (let ((num (parse-integer line :junk-allowed t)))
    (cons num (rest-of-line line 4))))

(defparameter *tsc-command-table*
  '((:AE+ "Refill ammo")
    (:AM+ "Get weapon X, add Y to max ammo (just adds ammo if you have the weapon)")
    (:AM- "Lose weapon X")
    (:AMJ "Jump to event Y if you have weapon X")
    (:ANP "Animate entity X with method Y in direction Z [entity type determines Y values?]")
    (:BOA "[animate boss entity?]")
    (:CLO "Close the text box (used after MSG/MS2/MS3)")
    (:CLR "Clear the text box (used after MSG/MS2/MS3)")
    (:CMP "Change map coords X:Y to tile Z")
    (:CMU "Change music to song X")
    (:CNP "Change entity X to entity type Y with direction Z")
    (:CPS "Stop propeller sound (used after SPS) (from helicopter cutscene after final battles)")
    (:CRE "Roll credits")
    (:CSS "Stop stream sound (used after SSS) (from River area)")
    (:DNA "[something to do with bosses]")
    (:DNP "Entity X is removed completely")
    (:ECJ "Jump to event Y if any entity with ID X is present")
    (:END "End scripted event")
    (:EQ+ "Add X to equip flag bytes")
    (:EQ- "Subtract X from equip flag bytes")
    (:ESC "Quit to title screen")
    (:EVE "Jump to event X (non-conditional)")
    (:FAC "Show face X in text box")
    (:FAI "Fade in with direction X")
    (:FAO "Fade out with direction X")
    (:FL+ "Set flag X")
    (:FL- "Clear flag X")
    (:FLA "Flash the screen")
    (:FLJ "Jump to event Y if flag X is set")
    (:FMU "Fade the music to a low volume (good to use before CMU)")
    (:FOB "[Focus view on boss entity X? why not use FON?], view movement takes Y ticks")
    (:FOM "Focus view on you (normal view), view movement takes X ticks (WARNING: speed 0000 crashes)")
    (:FON "Focus view on entity X, view movement takes Y ticks")
    (:FRE "Frees menu cursor [also used after ZAM for some reason?]")
    (:GIT "Show weapon/item X icon above text box - add 1000 to X for items - GIT0000 to hide")
    (:HMC "Removes main character entity (use SMC after)")
    (:INI "Resets memory and starts game from the beginning")
    (:INP "@")
    (:IT+ "Get item X")
    (:IT- "Lose item X")
    (:ITJ "Jump to event Y if you have item X")
    (:KEY "Hides status bars and locks out input to your character until END (used with MSG/MS2/MS3 and PRI)")
    (:LDP "Loads profile.dat into memory and starts game from save")
    (:LI+ "Restore X amount of health")
    (:ML+ "Max health increased X amount")
    (:MLP "Display map [how is this used without blanking screen while map is displayed?]")
    (:MM0 "Instantly halts your horizontal motion")
    (:MNA "Displays name of current map")
    (:MNP "Move entity X to coords Y:Z facing direction W")
    (:MOV "Move you to coords X:Y")
    (:MP+ "@")
    (:MPJ "[Jump to event X if map exists for current area?]")
    (:MS2 "Open invisible text box at top of screen (text follows)")
    (:MS3 "Open normal text box at top of screen (text follows)")
    (:MSG "Open normal text box (text follows)")
    (:MYB "Knocks you back from direction X (0000 knocked right, 0002 knocked left, any other just hops in place)")
    (:MYD "Make you face direction X")
    (:NCJ "Jump to event Y if any entity of type X is present")
    (:NOD "Text box wait for button press (used after MSG/MS2/MS3)")
    (:NUM "[used to output Y from AM+ as text, not sure exactly what it is]")
    (:PRI "Hides status bars and freezes game action until KEY or END (used with MSG/MS2/MS3)")
    (:PS+ "Set teleporter slot X to location Y")
    (:QUA "Shake the screen for X ticks")
    (:RMU "Restore music playback")
    (:SAT "Instant text display on all messages until END (glitches scrolling text)")
    (:SIL "Show illustration during credits (use CIL after)")
    (:SK+ "Set skipflag X (remains set until program exits, to avoid repeating cutscenes/dialogue after retrying)")
    (:SK- "@")
    (:SKJ "Jump to event Y if skipflag X is set")
    (:SLP "Teleporter location menu")
    (:SMC "Restores main character entity (used after HMC)")
    (:SMP "[do something with entity X? - only used before and after the Omega fight]")
    (:SNP "@[create enemy/entity type X?] at coords Y:Z with direction W")
    (:SOU "Play sound effect X")
    (:SPS "Start propeller sound (use CPS after) (from helicopter cutscene after final battles)")
    (:SSS "Start stream sound at pitch X (use CSS after) (from River area - normal pitch is 0400)")
    (:STC "Saves the current time to 290.rec")
    (:SVP "Save game")
    (:TAM "Trade weapon X for weapon Y, set max ammo to Z (max ammo 0000 = no change) (GLITCH: first weapon 0000)")
    (:TRA "Load map X, run event Y, transport you to coords Z:W")
    (:TUR "Instant text display [until what? CLR?] (used after MSG/MS2/MS3)")
    (:UNI "[0000 normal / 0001 zero-g movement, facing direction is locked (disables focus commands) (from Stream boss) / 0002 movement is locked, can still fire]")
    (:WAI "Pause script for X ticks")
    (:WAS "Pause script until your character touches the ground")
    (:XX1 "[shows distant view of island?]")
    (:YNJ "Ask yes or no, jump to event X if No")
    (:ZAM "All weapons drop to level 1")))
