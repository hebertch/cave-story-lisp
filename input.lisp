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
  mouse-wheel-dt)

(defstructure input
    "Input that is persistent through updates."
  (mouse-coords (zero-v))
  held-buttons
  held-keys
  held-joy-buttons
  joy-axis-x
  joy-axis-y
  (transient-input (make-transient-input)))

(defun ti-press-key (ti scancode)
  (modify-transient-input (ti)
    (pushnew scancode pressed-keys)))

(defun ti-release-key (ti scancode)
  (modify-transient-input (ti)
    (pushnew scancode released-keys)))

(defun ti-press-joy (ti num)
  (modify-transient-input (ti)
    (pushnew num pressed-joy-buttons)))

(defun ti-release-joy (ti num)
  (modify-transient-input (ti)
    (pushnew num released-joy-buttons)))

(defun ti-press-button (ti num)
  (modify-transient-input (ti)
    (pushnew num pressed-buttons)))

(defun ti-release-button (ti num)
  (modify-transient-input (ti)
    (pushnew num released-buttons)))

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

(defun gather-input (input)
  "Gather the input collected this frame into INPUT."
  (modify-input (input)
    (fnf transient-input #'clear-transient-input)
    (let (scancode)
      (loop until (= 0 (sdl:poll-event *event*)) do
	   (case (sdl:event-get-type *event*)
	     (:key-up
	      (setf scancode (sdl:keyboard-event-get-scancode *event*))
	      (setf transient-input (ti-release-key transient-input scancode))
	      (removef held-keys scancode))
	     (:key-down
	      (unless (sdl:keyboard-event-get-repeat *event*)
		(setf scancode (sdl:keyboard-event-get-scancode *event*))
		(setf transient-input (ti-press-key transient-input scancode))
		(pushnew scancode held-keys)))
	     (:joy-axis-motion
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		;; NOTE: Converts from value to :pos/:neg/nil
		(let* ((axis (sdl:event-get-joystick-axis *event*))
		       (value (sdl:event-get-joystick-axis-value *event*))
		       (sign (cond
			       ((plusp value) :positive)
			       ((minusp value) :negative)
			       (t nil))))
		  (case axis
		    (0 (setf (input-joy-axis-x input) sign))
		    (1 (setf (input-joy-axis-y input) sign))))))
	     (:joy-button-down
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		(let ((button (sdl:event-get-joystick-button *event*)))
		  (pushnew button held-joy-buttons)
		  (fnf transient-input (rcurry #'ti-press-joy button))
		  (when show-joy-buttons?
		    (format t "Button Pressed: ~A~%" button)))))
	     (:joy-button-up
	      (when (= 0 (sdl:event-get-joystick-which *event*))
		(let ((button (sdl:event-get-joystick-button *event*)))
		  (removef held-joy-buttons button)
		  (fnf transient-input (rcurry #'ti-release-joy button)))))
	     (:mouse-button-down
	      (let ((button (sdl:event-get-mouse-button *event*)))
		(fnf transient-input (rcurry #'ti-press-button button))
		(pushnew button held-buttons)))
	     (:mouse-button-up
	      (let ((button (sdl:event-get-mouse-button *event*)))
		(fnf transient-input (rcurry #'ti-release-button button))
		(removef button held-buttons)))
	     (:mouse-motion
	      (mvbind (x y) (sdl:event-get-mouse-xy *event*)
		(setf (input-mouse-coords input) (vector x y))))
	     (:mouse-wheel
	      (mvbind (x y) (sdl:event-get-mouse-xy *event*)
		(setf transient-input (modify-transient-input (transient-input)
					(setf mouse-wheel-dt (vector x y))))))
	     (:quit (quit)))))))

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
