(in-package :cave-story)

(defstruct (transient-input
	     (:conc-name ti-))
  "Input that occurs in a single frame"
  pressed-keys
  released-keys
  pressed-buttons
  released-buttons
  mouse-wheel-dt)

(defstruct input
  "Input that is persistent through updates."
  (mouse-coords (zero-v))
  held-buttons
  held-keys
  (transient-input (make-transient-input))
  (event (sdl:create-event)))

(defun clear-transient-input (ti)
  (nilf (ti-pressed-keys ti)
	(ti-released-keys ti)
	(ti-pressed-buttons ti)
	(ti-released-buttons ti)
	(ti-mouse-wheel-dt ti)))

(defun cleanup-input (input)
  (sdl:destroy-event (input-event input))
  (nilf (input-event input)))

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
