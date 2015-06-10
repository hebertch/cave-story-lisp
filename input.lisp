(in-package :cave-story)

(defun open-joystick ()
  (when (plusp (sdl:num-joysticks))
    (let ((idx 0))
      (let ((joystick (sdl:joystick-open 0)))
	(when (cffi:null-pointer-p joystick)
	  (format t "Failed to Open Joystick at index ~A. ~A~%" idx (sdl:get-error))
	  (nilf joystick))
	joystick))))

(defstruct (transient-input
	     (:conc-name ti-))
  "Input that occurs in a single frame"
  pressed-keys
  released-keys
  pressed-buttons
  released-buttons
  pressed-joy-buttons
  released-joy-buttons
  mouse-wheel-dt)

(defstruct input
  "Input that is persistent through updates."
  (mouse-coords (zero-v))
  held-buttons
  held-keys
  held-joy-buttons
  joy-axis-x
  joy-axis-y
  (transient-input (make-transient-input))
  (joystick (open-joystick))
  (event (sdl:create-event)))

(defun clear-transient-input (ti)
  (nilf (ti-pressed-keys ti)
	(ti-released-keys ti)
	(ti-pressed-buttons ti)
	(ti-released-buttons ti)
	(ti-mouse-wheel-dt ti)
	(ti-pressed-joy-buttons ti)
	(ti-released-joy-buttons ti)))

(defun cleanup-input (input)
  (sdl:destroy-event (input-event input))
  (nilf (input-event input))
  (awhen (input-joystick input)
    (when (sdl:joystick-get-attached it)
      (sdl:joystick-close it))
    (nilf (input-joystick input))))

(defparameter show-joy-buttons? nil)

(defun gather-input (input)
  "Gather the input collected this frame into INPUT."
  (let (scancode
	(ti (input-transient-input input))
	(event (input-event input)))
    (clear-transient-input ti)
    (loop until (= 0 (sdl:poll-event (input-event input))) do
	 (case (sdl:event-get-type event)
	   (:key-up
	    (setf scancode
		  (sdl:keyboard-event-get-scancode event))
	    (pushnew (sdl:keyboard-event-get-scancode event)
		     (ti-released-keys ti))
	    (removef (input-held-keys input) scancode))
	   (:joy-axis-motion
	    (when (= 0 (sdl:event-get-joystick-which event))
	      ;; NOTE: Converts from value to :pos/:neg/nil
	      (let* ((axis (sdl:event-get-joystick-axis event))
		     (value (sdl:event-get-joystick-axis-value event))
		     (sign (cond
			     ((plusp value) :positive)
			     ((minusp value) :negative)
			     (t nil))))
		(case axis
		  (0 (setf (input-joy-axis-x input) sign))
		  (1 (setf (input-joy-axis-y input) sign))))))
	   (:joy-button-down
	    (when (= 0 (sdl:event-get-joystick-which event))
	      (let ((button (sdl:event-get-joystick-button event)))
		(pushnew button (input-held-joy-buttons input))
		(pushnew button (ti-pressed-joy-buttons ti)))))
	   (:joy-button-up
	    (when (= 0 (sdl:event-get-joystick-which event))
	      (let ((button (sdl:event-get-joystick-button event)))
		(removef (input-held-joy-buttons input) button)
		(pushnew button (ti-released-joy-buttons ti))
		(when show-joy-buttons?
		  (format t "Button Pressed: ~A~%" button)))))
	   (:key-down
	    (unless (sdl:keyboard-event-get-repeat event)
	      (setf scancode (sdl:keyboard-event-get-scancode event))
	      (pushnew scancode (input-held-keys input))
	      (pushnew scancode (ti-pressed-keys ti))))
	   (:mouse-button-down
	    (pushnew (sdl:event-get-mouse-button event)
		     (ti-pressed-buttons ti)))
	   (:mouse-button-up
	    (removef (ti-released-buttons ti)
		     (sdl:event-get-mouse-button event)))
	   (:mouse-motion
	    (mvbind (x y) (sdl:event-get-mouse-xy event)
	      (setf (input-mouse-coords input) (vector x y))))
	   (:mouse-wheel
	    (mvbind (x y) (sdl:event-get-mouse-xy event)
	      (setf (ti-mouse-wheel-dt ti) (vector x y))))
	   (:quit (quit))))))

;; Individual key checks
(defun key-pressed? (input key)
  (find key (ti-pressed-keys (input-transient-input input))))
(defun key-released? (input key)
  (find key (ti-released-keys (input-transient-input input))))
(defun key-held? (input key)
  (find key (input-held-keys input)))

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
