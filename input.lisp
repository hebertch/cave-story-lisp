(in-package :cave-story)

(defstruct (transient-input
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

(defstruct input
  "Input that is persistent through updates."
  (mouse-coords (zero-v))
  held-buttons
  held-keys
  held-joy-buttons
  joy-axis-x
  joy-axis-y
  (transient-input (make-transient-input)))

(defvar *event*)
(defvar *joystick*)
(defparameter *show-joy-buttons?* nil)

(defun reset-transient-input (input)
  "Return a copy of input with the transient-input cleared."
  (let ((input (copy-structure input)))
    (setf (input-transient-input input) (make-transient-input))
    input))

(defun open-joystick! ()
  "Tries to open a joystick for reading input."
  (when (plusp (sdl:num-joysticks))
    (let ((idx 0))
      (let ((joystick (sdl:joystick-open 0)))
	(when (cffi:null-pointer-p joystick)
	  (warn "Failed to Open Joystick at index ~A. ~A~%"
		idx
		(sdl:get-error))
	  (nilf joystick))
	joystick))))

(defun init-input! ()
  (setf *event* (sdl:create-event)
	*joystick* (open-joystick!)))

(defun cleanup-input! ()
  (sdl:destroy-event *event*)
  (nilf *event*)
  (when *joystick*
    (when (sdl:joystick-get-attached *joystick*)
      (sdl:joystick-close *joystick*))
    (nilf *joystick*)))

(defun gather-transient-input! ()
  "Gather all input for a frame into a transient-input object."
  (let ((ti (make-transient-input)))
    ;; Gather all input for this frame into the transient input object.
    (loop until (= 0 (sdl:poll-event *event*)) do
	 (case (sdl:event-get-type *event*)
	   ;; Keyboard
	   (:key-up
	    (pushnew (sdl:keyboard-event-get-scancode *event*)
		     (ti-released-keys ti)))
	   (:key-down
	    (unless (sdl:keyboard-event-get-repeat *event*)
	      (pushnew (sdl:keyboard-event-get-scancode *event*)
		       (ti-pressed-keys ti))))

	   ;; Joystick
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
		  (0 (setf (ti-joy-axis-x ti) sign))
		  (1 (setf (ti-joy-axis-y ti) sign))))))
	   (:joy-button-down
	    (when (= 0 (sdl:event-get-joystick-which *event*))
	      (let ((button (sdl:event-get-joystick-button *event*)))
		(pushnew button (ti-pressed-joy-buttons ti))
		(when *show-joy-buttons?*
		  (format t "Button Pressed: ~A~%" button)))))
	   (:joy-button-up
	    (when (= 0 (sdl:event-get-joystick-which *event*))
	      (let ((button (sdl:event-get-joystick-button *event*)))
		(pushnew button (ti-released-joy-buttons ti)))))

	   ;; Mouse
	   (:mouse-button-down
	    (pushnew (sdl:event-get-mouse-button *event*)
		     (ti-pressed-buttons ti)))
	   (:mouse-button-up
	    (pushnew (sdl:event-get-mouse-button *event*)
		     (ti-released-buttons ti)))
	   (:mouse-motion
	    (mvbind (x y) (sdl:event-get-mouse-xy *event*)
	      (setf (ti-mouse-coords ti) (vector x y))))
	   (:mouse-wheel
	    (mvbind (x y) (sdl:event-get-mouse-xy *event*)
	      (setf (ti-mouse-wheel-dt ti) (vector x y))))

	   ;; Window-Manager
	   (:quit (quit))))
    ti))

(defun gather-held (held pressed released)
  "Combines the currently held events with the new pressed/released events."
  (set-difference (union held pressed) released))

(defun combine-transient-inputs (ti1 ti2)
  "Combine an two transient inputs into one.
Favors ti2 as the most recent input."
  (make-transient-input
   :pressed-keys
   (union (ti-pressed-keys ti1) (ti-pressed-keys ti2))
   :released-keys
   (union (ti-released-keys ti1) (ti-released-keys ti2))
   :pressed-buttons
   (union (ti-pressed-buttons ti1) (ti-pressed-buttons ti2))
   :released-buttons
   (union (ti-released-buttons ti1) (ti-released-buttons ti2))
   :pressed-joy-buttons
   (union (ti-pressed-joy-buttons ti1) (ti-pressed-joy-buttons ti2))
   :released-joy-buttons
   (union (ti-released-joy-buttons ti1) (ti-released-joy-buttons ti2))
   :mouse-wheel-dt
   (union (ti-mouse-wheel-dt ti1) (ti-mouse-wheel-dt ti2))
   :mouse-coords
   (if (ti-mouse-coords ti2)
       (ti-mouse-coords ti2)
       (ti-mouse-coords ti1))
   :joy-axis-x
   (if (ti-joy-axis-x ti2)
       (ti-joy-axis-x ti2)
       (ti-joy-axis-x ti1))
   :joy-axis-y
   (if (ti-joy-axis-y ti2)
       (ti-joy-axis-y ti2)
       (ti-joy-axis-y ti1))))

(defun gather-input (input transient-input)
  "Combine the existing input with the last frame's transient-input."
  (make-input
   :transient-input (combine-transient-inputs
		     (input-transient-input input)
		     transient-input)

   :held-joy-buttons
   (gather-held (input-held-joy-buttons input)
		(ti-pressed-joy-buttons transient-input)
		(ti-released-joy-buttons transient-input))
   :held-buttons
   (gather-held (input-held-buttons input)
		(ti-pressed-buttons transient-input)
		(ti-released-buttons transient-input))
   :held-keys
   (gather-held (input-held-keys input)
		(ti-pressed-keys transient-input)
		(ti-released-keys transient-input))

   :joy-axis-x
   (if (ti-joy-axis-x transient-input)
       (ti-joy-axis-x transient-input)
       (input-joy-axis-x input))
   :joy-axis-y
   (if (ti-joy-axis-y transient-input)
       (ti-joy-axis-y transient-input)
       (input-joy-axis-y input))
   :mouse-coords
   (if (ti-mouse-coords transient-input)
       (ti-mouse-coords transient-input)
       (input-mouse-coords input))))

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
