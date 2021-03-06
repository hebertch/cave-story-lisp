(declaim (optimize debug))
(swank:set-default-directory "/home/chebert/Projects/lisp/cave-story")
(load "load.lisp")
(in-package :cave-story)

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

(defvar* *rolling-avg* (alist :num-entries 360))

(rolling-average-time *rolling-avg* (sleep 1))

(drawings-rect (aval (first (ncompile-drawings (stage-drawing (cave-stage)))) :drawings))

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

(defvar* *tsc* (decrypt-tsc-file "./content/stages/Cave.tsc"))
(parse-decrypted-tsc-script *tsc*)
