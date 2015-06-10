(in-package :cave-story)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar subsystems nil)

  (defun interface-forms-name (interface)
    (symbolicate interface '-forms))

  (defun lambda-form (interface)
    `(list* 'lambda (list ,@ (cdr interface))
	    ,(interface-forms-name (car interface)))))

(defmacro def-subsystem (name (&rest interface) update-args &body update-forms)
  "Creates a subsytem of NAME.
DEAD?-FN is automatically part of the interface, and is used to determine whether the entity
should be removed from the REGISTRY.
REGISTER-name is the interface to add to name-REGISTRY.
DEF-ENTITY-name is the interface to be used with DEF-ENTITY. Expects DEAD?-FN to exist.
UPDATE-name-SUBSYSTEM evaluates UPDATE-FORMS given INTERFACE and UPDATE-ARGS."

  (let ((registry (symbolicate name '-registry))
	(register-name (symbolicate 'register- name))
	(update-name (symbolicate 'update- name '-subsystem))
	(dead-interface (mapcar #'alexandria:ensure-list (cons 'dead?-fn interface)))
	(interface (mapcar #'alexandria:ensure-list interface))
	(def-entity-name (symbolicate 'def-entity- name)))
    (with-gensyms (make-name conc-name entity-name)
      `(progn
	 (defvar ,registry nil)
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (alexandria:assoc-value subsystems (intern (string ',name) :keyword) :test #'eq)
		 (lambda () (nilf ,registry))))
	 (defstruct (,(gensym)
		      (:constructor ,make-name ,(mapcar #'car dead-interface))
		      (:conc-name ,(string conc-name))
		      (:copier nil))
	   ,@ (mapcar #'car dead-interface))

	 (defun ,register-name (&key ,@ (mapcar #'car dead-interface))
	   (push (,make-name ,@(mapcar #'car dead-interface)) ,registry))

	 (defmacro ,def-entity-name ,(loop for i in interface
					appending `(,(list (cdr i)
							   '&body
							   (interface-forms-name (car i)))))
	   (list ',register-name :dead?-fn 'dead?-fn
		 ,@(loop for i in interface
		      appending `(,(alexandria:make-keyword (car i))
				   ,(lambda-form i)))))

	 (defun ,update-name ,update-args
	   (setf ,registry (remove-if (lambda (p)
					(awhen (,(symbolicate conc-name 'dead?-fn) p)
					  (funcall it)))
				      ,registry))
	   (dolist (,entity-name ,registry)
	     (let ,(loop for i in dead-interface
		      collecting
			(list (car i) (list (symbolicate conc-name (car i)) entity-name)))
	       (declare (ignorable ,@ (mapcar #'car dead-interface)))
	       ,@update-forms)))
	 (values ',registry ',register-name ',update-name)))))

(defmacro def-simple-subsystem (name fn-name)
  `(def-subsystem ,name (,fn-name) ()
     (funcall ,fn-name)))

(def-simple-subsystem timer update-fn)
(def-simple-subsystem physics physics-fn)
(def-simple-subsystem ai ai-fn)
(def-simple-subsystem drawable draw-fn)

(def-subsystem stage-collision ((collision-fn stage)) (stage)
  (funcall collision-fn stage))

(def-subsystem dynamic-collision (rect-fn vel-fn (react-fn side player-collision-rect player)) (player)
  ;; TODO: Merge with stage collisions.
  (dolist (side collision-order)
    (let* ((rect (funcall rect-fn))
	   (player-collision-rect (cdr (assoc side player-collision-rectangles-alist)))
	   (player-rect (rect-offset player-collision-rect (player-pos player))))
      (draw-rect rect blue :layer :debug-dynamic-collision)
      (draw-rect player-rect green :layer :debug-dynamic-collision)
      (when (rects-collide? rect player-rect)
	(draw-rect player-rect green :layer :debug-dynamic-collision :filled? t)
	(draw-rect rect yellow :layer :debug-dynamic-collision :filled? t)
	(funcall react-fn side player-collision-rect player)))))

(def-subsystem damageable (rect-fn (hit-fn bullet-hit-amt))
    ;; NOTE: UPDATE-DAMAGEABLE-SUBSYSTEM is designed to be called by UPDATE-BULLET-SUBSYSTEM
    (bullet-rect bullet-hit-amt bullet-hit-fn bullet-dead?-fn)
  (unless (funcall bullet-dead?-fn)
    (let ((rect (funcall rect-fn)))
      (draw-rect bullet-rect green :layer :debug-damageable)
      (draw-rect rect blue :layer :debug-damageable)
      (when (rects-collide? rect bullet-rect)
	(draw-rect bullet-rect yellow :layer :debug-damageable :filled? t)
	(draw-rect rect yellow :layer :debug-damageable :filled? t)
	(funcall bullet-hit-fn)
	(funcall hit-fn bullet-hit-amt)))))

(def-subsystem bullet (rect-fn hit-fn damage-amt-fn) ()
  (update-damageable-subsystem
   (funcall rect-fn) (funcall damage-amt-fn) hit-fn dead?-fn))

(def-subsystem pickup (rect-fn kill-fn)
    (player)
  (let ((rect (funcall rect-fn))
	(player-rect  (player-damage-collision-rect player)))
    (draw-rect rect green :layer :debug-pickup)
    (draw-rect player-rect blue :layer :debug-pickup)
    (when (rects-collide? rect player-rect)
      (draw-rect rect yellow :layer :debug-pickup :filled? t)
      (draw-rect player-rect yellow :layer :debug-pickup :filled? t)
      (funcall kill-fn))))

(def-subsystem damage-collision (rect-fn dmg-amt-fn) (player hud)
  (let ((rect (funcall rect-fn))
	(player-rect (player-damage-collision-rect player)))
    (draw-rect rect red :layer :debug-damage-collision)
    (draw-rect player-rect blue :layer :debug-damage-collision)
    (when (rects-collide? rect player-rect)
      (player-take-damage player hud (funcall dmg-amt-fn))
      (draw-rect rect magenta :layer :debug-damage-collision :filled? t)
      (draw-rect player-rect magenta :layer :debug-damage-collision :filled? t))))
