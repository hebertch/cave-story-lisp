(in-package :cave-story)

(defun open-joystick ()
  (when (plusp (sdl:num-joysticks))
    (let ((idx 0))
      (let ((joystick (sdl:joystick-open 0)))
	(when (cffi:null-pointer-p joystick)
	  (format t "Failed to Open Joystick at index ~A. ~A~%" idx (sdl:get-error))
	  (nilf joystick))
	joystick))))

(defstructure (transient-input
	       (:conc-name ti-))
    "Input that occurs in a single frame"
  pressed-keys
  released-keys
  pressed-buttons
  released-buttons
  pressed-joy-buttons
  released-joy-buttons
  mouse-wheel-dt
  mouse-coords
  joy-axis-x
  joy-axis-y)

(defstructure input
    "Input that is persistent through updates."
  (mouse-coords (zero-v))
  held-buttons
  held-keys
  held-joy-buttons
  joy-axis-x
  joy-axis-y
  (transient-input (make-transient-input)))

(defun clear-transient-input (ti)
  (modify-transient-input (ti)
    (nilf pressed-keys
	  released-keys
	  pressed-buttons
	  released-buttons
	  mouse-wheel-dt
	  pressed-joy-buttons
	  released-joy-buttons)))

(defvar *event*)
(defvar *joystick*)
(defun init-input ()
  (setf *event* (sdl:create-event)
	*joystick* (open-joystick)))

(defun cleanup-input ()
  (sdl:destroy-event *event*)
  (nilf *event*)
  (awhen *joystick*
    (when (sdl:joystick-get-attached it)
      (sdl:joystick-close it))
    (nilf *joystick*)))

(defparameter show-joy-buttons? nil)

(defun gather-transient-input ()
  (let ((res (make-transient-input)))
    (with-transient-input-slots (res)
      (loop until (= 0 (sdl:poll-event *event*)) do
	   (case (sdl:event-get-type *event*)
	     (:key-up
	      (pushnew (sdl:keyboard-event-get-scancode *event*) released-keys))
	     (:key-down
	      (unless (sdl:keyboard-event-get-repeat *event*)
		(pushnew (sdl:keyboard-event-get-scancode *event*) pressed-keys)))
	     (:joy-axis-motion
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		;; NOTE: Converts from value to :pos/:neg/nil
		(let* ((axis (sdl:event-get-joystick-axis *event*))
		       (value (sdl:event-get-joystick-axis-value *event*))
		       (sign (cond
			       ((plusp value) :positive)
			       ((minusp value) :negative)
			       (t :zero))))
		  (case axis
		    (0 (setf joy-axis-x sign))
		    (1 (setf joy-axis-y sign))))))
	     (:joy-button-down
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		(let ((button (sdl:event-get-joystick-button *event*)))
		  (pushnew button pressed-joy-buttons)
		  (when show-joy-buttons?
		    (format t "Button Pressed: ~A~%" button)))))
	     (:joy-button-up
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		(let ((button (sdl:event-get-joystick-button *event*)))
		  (pushnew button released-joy-buttons))))
	     (:mouse-button-down
	      (pushnew (sdl:event-get-mouse-button *event*) pressed-buttons))
	     (:mouse-button-up
	      (pushnew (sdl:event-get-mouse-button *event*) released-buttons))
	     (:mouse-motion
	      (mvbind (x y) (sdl:event-get-mouse-xy *event*)
		(setf mouse-coords (vector x y))))
	     (:mouse-wheel
	      (mvbind (x y) (sdl:event-get-mouse-xy *event*)
		(setf mouse-wheel-dt (vector x y))))
	     (:quit (quit)))))
    res))

(defun gather-held (held pressed released)
  (unionf held pressed)
  (set-differencef held released))

(defmacro gather-heldf (held pressed released)
  `(setf ,held (gather-held ,held ,pressed ,released)))

(defun gather-input (input transient-input)
  "Gather the input collected this frame into INPUT."
  (modify-input (input i-)
    (with-transient-input-slots (transient-input)
      (modify-transient-input (i-transient-input iti-)
	(unionf iti-pressed-keys pressed-keys)
	(unionf iti-pressed-joy-buttons pressed-joy-buttons)
	(unionf iti-pressed-buttons pressed-buttons)
	(unionf iti-released-keys released-keys)
	(unionf iti-released-joy-buttons released-joy-buttons)
	(unionf iti-released-buttons released-buttons))
      (gather-heldf i-held-joy-buttons pressed-joy-buttons released-joy-buttons)
      (gather-heldf i-held-buttons pressed-buttons released-buttons)
      (gather-heldf i-held-keys pressed-keys released-keys)
      (when joy-axis-x
	(setf i-joy-axis-x joy-axis-x))
      (when joy-axis-y
	(setf i-joy-axis-y joy-axis-y))
      (when mouse-coords
	(setf i-mouse-coords mouse-coords)))))

;; Individual key checks
(defun key-pressed? (input key)
  (find key (ti-pressed-keys (input-transient-input input))))
(defun key-released? (input key)
  (find key (ti-released-keys (input-transient-input input))))
(defun key-held? (input key)
  (find key (input-held-keys input)))

(defun joy-key->num (keysym)
  (position keysym '(:a :b :x :y :l :r :select :start)))

(defun joy-pressed? (input keysym)
  (let ((num (joy-key->num keysym)))
    (find num (ti-pressed-joy-buttons (input-transient-input input)))))

(defun joy-held? (input keysym)
  (let ((num (joy-key->num keysym)))
    (find num (input-held-joy-buttons input))))

;; Return lists:
(defun pressed-keys (input &rest keys)
  (mapcar (curry #'key-pressed? input) keys))
(defun released-keys (input &rest keys)
  (mapcar (curry #'key-released? input) keys))
(defun held-keys (input &rest keys)
  (mapcar (curry #'key-held? input) keys))

(defun any? (lst)
  (remove-if #'null lst))
(defun all? (lst)
  (not (find #'null lst)))
