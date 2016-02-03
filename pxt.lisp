;;;; pxt.lisp
(in-package :cave-story)

;; Since I don't want to try to recreate pxt files
;; exactly, this is totally unused. Kept it around
;; because it is kind of neat.


(defparameter *pxt-wave-models*
  '((0 :sine)
    (1 :triangle)
    (2 :saw-up)
    (3 :saw-down)
    (4 :square)
    (5 :random))
  "List of (index . wave-model).")

(defparameter *pxt-channel-fields* 
  '((:use :bool "Whether the channel/track should be used.")
    (:size :int "Size in bytes of the sound sample. All frequencies are
relative to the size (not 1 second!), where 1 second of sound is 22050 bytes.
So at size=22050 440 Hz is A4, but at size=11025 440Hz is A5, since it is now
oscillating twice as fast.")
    (:main-model :wave-model "Wave model to use. Sin/triangle/etc.")
    (:main-freq :float "Frequency. 440 Hz is A4.")
    (:main-top :int "Amplitude of wave on a scale of 0 to 63. At 63, with no modulation,
the volume is 1/2 a decibel.")
    (:main-offset :int "The x offset of the wave.")
    (:pitch-model :wave-model "Wave model to use for the pitch modulation.
Formula for pitch is different depending on whether the pitch wave is above
or below 0.
Let f' be the frequency after modulation, x be the pitch modulation value
for a given time, and f be the base frequency.
f' = (1 + x/32) * f    if x >= 0
f' = (1 - x/256) * f   if x < 0")
    (:pitch-freq :float "Frequency of the pitch modulation wave.")
    (:pitch-top :int "Amplitude of the pitch modulation.")
    (:pitch-offset :int "X offset of the pitch modulation wave.")
    (:volume-model :wave-model "Wave model describing the volume modulation.")
    (:volume-freq :float "Frequency of the volume modulation.")
    (:volume-top :int "Amplitude [0-63] of the volume modulation.
This form a fractional amount (out of 63) by which to modulate the volume.
I.e. 63 could result in doubling/canceling out the audio and
32 could result in volume increasing/decreasing by half.")
    (:volume-offset :int "X offset of the volume modulation wave.")
    (:initial-y :int "[0-63]. Initial value of base volume.
These fields (ax-cy) create a volume envelope with linear interpolation
between each point. This is calculated before the volume-model.")
    (:ax :int "[0-255]. X value of when volume should be ay.")
    (:ay :int "[0-63]. Value of x at time ax.")
    (:bx :int "[ax-255].")
    (:by :int "[0-63]")
    (:cx :int "[bx-255]")
    (:cy :int "[0-63]"))
  "Ordered list of channel/track fields for a pxt file. 
Each field is a (key parse-type description).")

(defun read-pxt-file (path)
  "Reads a pixtone (sound effect) file. Returns a list of tracks."
  ;; The pxt file is a human readable : and newline separated form,
  ;; followed by comma-separated values of all the same values.
  ;; Because the csv are somewhat unreliable (might have extra curly-braces),
  ;; I'll use the human readable values.

  ;; Pxt files are 4 tracks. The tracks are only mixed if their :use
  ;; flag is set, so discard any track with the use flag unset.
  ;; The order of the tracks is irrelevant.
  ;; The fields always come in the same order, so we do not check the
  ;; contents of the field name.
  (let* ((lines
	 (split-sequence:split-sequence
	  #\newline
	  (remove #\return (read-file-into-string path))
	  :remove-empty-subseqs t))
	 (field-lines (subseq lines 0 (- (length lines) 4)))
	 (fields (mapcar (comp second #_(split-sequence:split-sequence #\: _))
			 field-lines))
	 (num-fields (length *pxt-channel-fields*))
	 (track1-fields (subseq fields 0 num-fields))
	 (track2-fields (subseq fields num-fields (* 2 num-fields)))
	 (track3-fields (subseq fields (* 2 num-fields) (* 3 num-fields)))
	 (track4-fields (subseq fields (* 3 num-fields))))
    (remove-if-not
     (lambda (c) (aval c :use))
     (list
      (mapcar #'parse-pxt-channel-field track1-fields *pxt-channel-fields*)
      (mapcar #'parse-pxt-channel-field track2-fields *pxt-channel-fields*)
      (mapcar #'parse-pxt-channel-field track3-fields *pxt-channel-fields*)
      (mapcar #'parse-pxt-channel-field track4-fields *pxt-channel-fields*)))))

(defun parse-pxt-channel-field (val field)
  "Parse a pxt colon-separated channel field val,
given that it is expected to be associated with the channel field.
Field is a (key type) list."
  (let ((key (first field))
	(type (second field)))
    (cons
     key
     (ecase type
       (:int (parse-integer val))
       (:float (read-from-string val))
       (:bool (not (zerop (parse-integer val))))
       (:wave-model (let ((idx (parse-integer val)))
		      (first (assoc-value *pxt-wave-models* idx))))))))

(defun pxt-tracks->wav-bytes (tracks)
  "Converts tracks from a pxt file into an vector of bytes
for .wav form playback."
  (let ((fn (pxt-tracks-function tracks)))
    (map 'vector
	 (lambda (x)
	   (+ 128 (floor (* 127 (funcall fn x)))))
	 (iota (aval (first tracks) :size)))))

(defun pxt-tracks-function (tracks)
  "Combines the pxt-track functions of tracks."
  (let ((fns (mapcar #'pxt-track-function tracks)))
    (lambda (x)
      (let ((res 0))
	(dolist (fn fns)
	  (setq res (+ res (funcall fn x))))
	res))))

(defun volume-envelope-function (track)
  "Returns the volume envelope as a function of samples."
  ;; ax, bx, cx are all fractions of size out of 255.
  ;; initial-y, ay, by, cy are all fractions out of 63.
  (let* ((size (aval track :size))
	 (ax (* size (/ (aval track :ax) 255)))
	 (bx (* size (/ (aval track :bx) 255)))
	 (cx (* size (/ (aval track :cx) 255)))
	 (initial-y (/ (aval track :initial-y) 63))
	 (ay (/ (aval track :ay) 63))
	 (by (/ (aval track :by) 63))
	 (cy (/ (aval track :cy) 63)))
    (lambda (x)
      (cond ((< x ax)
	     (lerp (/ x ax) initial-y ay))
	    ((< x bx)
	     (lerp (/ (- x ax) (- bx ax)) ay by))
	    ((< x cx)
	     (lerp (/ (- x bx) (- cx bx)) by cy))
	    (t
	     (lerp (/ (- x cx) (- 255 cx)) cy 0))))))

(defun pxt-track-function (track)
  "Return a function that when given a byte offset returns
the sound value."
  (let* ((size (aval track :size))
	 (pitch-wave (pitch-wave-function track))
	 (volume-wave (volume-wave-function track))
	 (volume-envelope (volume-envelope-function track)))
    (lambda (x)
      (let* ((pitch (funcall pitch-wave x))
	     (main-freq (aval track :main-freq))
	     (freq (cond ((< pitch 0) (* main-freq (+ 1 pitch)))
			 (t (* main-freq (- 1 (/ pitch 8))))))
	     (volume (funcall volume-wave x))
	     (main-top (aval track :main-top))
	     (top (min (funcall volume-envelope x)
		       (+ main-top (* volume main-top))))
	     (main-wave (pxt-wave-function size
					   (aval track :main-model)
					   freq
					   top
					   (aval track :main-offset))))
	(/ (funcall main-wave x) 2)))))

(defun pxt-wave-function (size model freq top offset)
  "Generates a pixtone wave function f that calculates the
sound at a given byte offset (between 0 and the size).
Freq is the number of oscillations PER size (not per second.)
Offset is a fractional amount out of 256 of a period to offset the wave."
  (if (= 0 freq)
      (constantly 0)
      (let* ((amp (/ top 63))
	     (period (/ size freq))
	     (off (* period offset (/ 256))))
	(lambda (x-amt)
	  (let ((x (+ off x-amt)))
	    (ecase model
	      (:square
	       (cond ((zerop (mod (floor x (/ period 2)) 2)) amp)
		     (t (- amp))))
	      (:sine
	       (* amp (sin (* x 2 pi (/ period)))))
	      (:saw-up
	       (+ (- amp) (* 2 amp (/ period) (mod x period))))
	      (:saw-down
	       (+ amp (- (* 2 amp (/ period) (mod x period)))))
	      (:triangle
	       (let* ((p/4 (/ period 4))
		      (quadrant (mod (floor x p/4) 4))
		      (osc-start (* period (floor x period)))
		      (slope (* 4 amp (/ period))))
		 (cond ((or (= quadrant 1) (= quadrant 2))
			(+ (* (- slope) (- x (+ osc-start p/4)))
			   amp))
		       ((= quadrant 0)
			(+ (* slope (- x (- osc-start p/4)))
			   (- amp)))
		       (t
			(+ (* slope (- x (+ osc-start (* 3 p/4))))
			   (- amp))))))
	      (:random
	       (* amp (- (random 2.0) 1.0)))))))))

(defun plot-waves! (wave-models)
  "Plot an example wave of of each wave-model in wave-models."
  (let* ((wave-function #_(pxt-wave-function 1000 _ 1.0 32 0))
	 (x (map 'vector (lambda (x) x) (iota 1000)))
	 (w (cl-plplot:basic-window)))
    (dolist (wave wave-models)
      (cl-plplot:add-plot-to-window
       w
       (cl-plplot:new-x-y-plot
	x
	(map 'vector (funcall wave-function wave) x))))
    (cl-plplot:render w "xwin"))
  (values))

(defun plot-all-waves! ()
  "Plot an example of each wave model."
  (plot-waves! (mapcar #'second *pxt-wave-models*)))

(defun plot-offsets! ()
  "Creates a plot of two wave functions with different offsets."
  (let* ((wave-function #_(pxt-wave-function 22050 :saw-up 440.0 32 _))
	 (x (map 'vector (lambda (x) x) (iota 1000)))
	 (w (cl-plplot:basic-window)))
    (cl-plplot:add-plot-to-window
     w
     (cl-plplot:new-x-y-plot
      x
      (map 'vector (funcall wave-function 0) x)))
  
    (cl-plplot:add-plot-to-window
     w
     (cl-plplot:new-x-y-plot
      x
      (map 'vector (funcall wave-function 128) x)))
    (cl-plplot:render w "xwin"))
  (values))

(defun pitch-wave-function (track)
  "Returns the function to compute the pitch modulation wave of a given track."
  (pxt-wave-function (aval track :size)
		     (aval track :pitch-model)
		     (aval track :pitch-freq)
		     (aval track :pitch-top)
		     (aval track :pitch-offset)))
(defun volume-wave-function (track)
  "Returns the function to compute the volume modulation wave of a given track."
  (pxt-wave-function (aval track :size)
		     (aval track :volume-model)
		     (aval track :volume-freq)
		     (aval track :volume-top)
		     (aval track :volume-offset)))

(defun plot-pxt-track! (track)
  "Plots a track of a .pxt file using plPlot."
  (let* ((track-function (pxt-track-function track))
	 (x (map 'vector #'identity (iota (aval track :size))))
	 (w (cl-plplot:basic-window)))
    (cl-plplot:add-plot-to-window
     w
     (cl-plplot:new-x-y-plot
      x
      (map 'vector track-function x)))
    (cl-plplot:render w "xwin"))
  (values))

(defun string-bytes (str)
  (map 'vector #'char-code str))

(defun uint32-bytes (u)
  "Return byte sequence (little-endian) of a uint32 u."
  (let ((bytes (make-array 4)))
    (setf (aref bytes 0) (ldb (byte 8 0) u))
    (setf (aref bytes 1) (ldb (byte 8 8) u))
    (setf (aref bytes 2) (ldb (byte 8 16) u))
    (setf (aref bytes 3) (ldb (byte 8 24) u))
    bytes))

(defun uint16-bytes (u)
  "Return byte sequence (little-endian) of a uint16 u."
  (let ((bytes (make-array 2)))
    (setf (aref bytes 0) (ldb (byte 8 0) u))
    (setf (aref bytes 1) (ldb (byte 8 8) u))
    bytes))

(defun wavefile-bytes (data)
  "Given the 8-bit data bytes, create the wave file bytes."
  (let ((contents
	 (concatenate 'vector
		      (string-bytes "WAVE")
		      (string-bytes "fmt ")
		      ;; fmt-length
		      (uint32-bytes 16)
		      ;; audio-format
		      (uint16-bytes 1)
		      ;; num-channels
		      (uint16-bytes 1)
		      ;; Samples/Second
		      (uint32-bytes 22050)
		      ;; byte-rate
		      (uint32-bytes 22050)
		      ;; block-align
		      (uint16-bytes 1)
		      ;; bits-per-sample
		      (uint16-bytes 8)
		      ;; Data-tag
		      (string-bytes "data")
		      ;; data-length
		      (uint32-bytes (length data))
		      ;; data
		      data)))
    (concatenate 'vector
		 (string-bytes "RIFF")
		 ;; Riff-length
		 (uint32-bytes (length contents))
		 contents)))

(defun pxt-file->wave-file! (pxt-path wave-path)
  "Read a .pxt file from pxt-path and write it to wave-path.
Overwrites wave-path if it exists."
  (with-open-file (stream wave-path :direction :output
			  :element-type '(unsigned-byte 8)
			  :if-exists :supersede)
    (write-sequence
     (wavefile-bytes (pxt-tracks->wav-bytes (read-pxt-file pxt-path)))
     stream))
  :done)

(defun plot-pxt! (path)
  (plot-pxt-track! (first (read-pxt-file path))))

(pxt-file->wave-file! "./content/pxt/fx46.pxt" "./test.wav")

#+nil
(play-sound-file "~/Projects/lisp/cave-story/test.wav")

(defun rand-next (seed)
  (setq seed (* seed #x343fd))
  (setq seed (ldb (byte 32 0) seed))
  (setq seed (+ seed #x269ec3))
  (setq seed (ldb (byte 32 0) seed))
  seed)

(defvar *pxt-rand-seed* 0)
(defun pxt-rand-next ()
  (setq *pxt-rand-seed* (rand-next *pxt-rand-seed*))
  (ldb (byte 16 0) (logand (ash *pxt-rand-seed* -16) #x7fff)))

(defun generate-rand-model ()
  (setq *pxt-rand-seed* 0)
  (collect
      #_(let ((k (u8->s8 (ldb (byte 8 0) (pxt-rand-next)))))
	  (when (< k 0)
	    (incf k))
	  (ash k -1))
      256))

(defun u8->s8 (byte)
  "Convert an unsigned byte to a two's complement signed byte."
  (if (> byte 127)
      (- byte 256)
      byte))

(defvar *seed* 0)
(defun seed-rand (seed)
  (setq *seed* (ldb (byte 32 0) seed)))

(defun get-rand ()
  (setq *seed* (rand-next *seed*)))

(defun random-range (min max)
  (when (< max min)
    (warn "Max is less than min.")
    (let ((tmp max))
      (setq max min
	    min tmp)))
  (let ((range (- max min)))
    (+ min (mod (get-rand) (+ range 1)))))