(in-package :cave-story)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar registry-syms nil)

  (defun interface-forms-name (interface)
    (symbolicate interface '-forms))

  (defun lambda-form (interface)
    `(list* 'lambda (list ,@ (cdr interface))
	    ,(interface-forms-name (car interface)))))

(defvar entity-system-type :game)

(defmacro def-subsystem (name update-args &body update-forms)
  "Creates a subsytem of NAME.
REGISTER-name is the interface to add to name-REGISTRY.
DEF-ENTITY-name is the interface to be used with DEF-ENTITY.
UPDATE-name-SUBSYSTEM evaluates UPDATE-FORMS given INTERFACE and UPDATE-ARGS."

  (let ((registry (symbolicate name '-registry))
	(register-name (symbolicate 'register- name))
	(update-name (symbolicate 'update- name '-subsystem)))
    (with-gensyms (entry-name)
      `(progn
	 (defvar ,registry nil)
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (pushnew ',registry registry-syms))

	 (defun ,register-name (system-type id)
	   (push (cons system-type id) ,registry))

	 (defun ,update-name ,(cons 'active-entity-systems update-args)
	   (setf ,registry (remove-if (lambda (p)
					(dead? (estate (cdr p))))
				      ,registry))
	   (dolist (,entry-name ,registry)
	     (when (member (car ,entry-name) active-entity-systems)
	       (let ((entity-id (cdr ,entry-name)))
		 ,@update-forms))))

	 (values ',registry ',register-name ',update-name)))))

(defun replace-entity-state (entity-id fn)
  (estate-set entity-id (funcall fn (estate entity-id))))

(defgeneric physics (obj))
(def-subsystem physics ()
  (replace-entity-state entity-id #'physics))

(defgeneric ai (obj))
(def-subsystem ai ()
  (replace-entity-state entity-id #'ai))

(defgeneric draw (obj))
(def-subsystem drawable ()
  (draw (estate entity-id)))

(defgeneric stage-collision (obj stage))
(def-subsystem stage-collision (stage)
  (replace-entity-state entity-id (rcurry #'stage-collision stage)))

(defgeneric input (obj input))
(def-subsystem input (input)
  (replace-entity-state entity-id (rcurry #'input input)))

(defgeneric dynamic-collision-vel (obj))
(defgeneric dynamic-collision-react (obj side player-collision-rect player))

(def-subsystem dynamic-collision (player)
  ;; TODO: Merge with stage collisions.
  (dolist (side collision-order)
    (let* ((state (estate entity-id))
	   (rect (dynamic-collision-rect state))
	   (player-collision-rect (cdr (assoc side player-collision-rectangles-alist)))
	   (player-rect (rect-offset player-collision-rect (player-pos (player-state player)))))
      (draw-rect rect blue :layer :debug-dynamic-collision)
      (draw-rect player-rect green :layer :debug-dynamic-collision)
      (when (rects-collide? rect player-rect)
	(draw-rect player-rect green :layer :debug-dynamic-collision :filled? t)
	(draw-rect rect yellow :layer :debug-dynamic-collision :filled? t)
	(estate-set player (dynamic-collision-react state side player-collision-rect player))))))

(defgeneric damageable-rect (obj))
(defgeneric damageable-hit-react (obj bullet-hit-amt))

(defgeneric bullet-rect (obj))
(defgeneric bullet-hit-react (obj))
(defgeneric bullet-damage-amt (obj))

(def-subsystem damageable (bullet-id)
  ;; NOTE: UPDATE-DAMAGEABLE-SUBSYSTEM is designed to be called by UPDATE-BULLET-SUBSYSTEM
  (unless (dead? (estate bullet-id))
    (let ((bullet-rect (bullet-rect (estate bullet-id)))
	  (bullet-hit-amt (bullet-damage-amt (estate bullet-id)))
	  (rect (damageable-rect (estate entity-id))))
      (draw-rect bullet-rect green :layer :debug-damageable)
      (draw-rect rect blue :layer :debug-damageable)
      (when (rects-collide? rect bullet-rect)
	(draw-rect bullet-rect yellow :layer :debug-damageable :filled? t)
	(draw-rect rect yellow :layer :debug-damageable :filled? t)
	(replace-entity-state entity-id (rcurry #'damageable-hit-react bullet-hit-amt))
	(replace-entity-state bullet-id #'bullet-hit-react)))))

(def-subsystem bullet ()
  (update-damageable-subsystem active-entity-systems entity-id))

(defgeneric pickup-rect (obj))
(defgeneric pickup-kill (obj))
(defgeneric pickup-data (obj))

(def-subsystem pickup (player)
  (let ((rect (pickup-rect (estate entity-id)))
	(player-rect  (player-damage-collision-rect (player-state player))))
    (draw-rect rect green :layer :debug-pickup)
    (draw-rect player-rect blue :layer :debug-pickup)
    (when (rects-collide? rect player-rect)
      (draw-rect rect yellow :layer :debug-pickup :filled? t)
      (draw-rect player-rect yellow :layer :debug-pickup :filled? t)
      (replace-entity-state player (lambda (player) (player-pickup player (pickup-data (estate entity-id)))))
      (replace-entity-state entity-id #'pickup-kill))))

(defgeneric damage-collision-rect (obj))
(defgeneric damage-collision-amt (obj))

(def-subsystem damage-collision (player)
  (let ((rect (damage-collision-rect (estate entity-id)))
	(player-rect (player-damage-collision-rect (player-state player))))
    (draw-rect rect red :layer :debug-damage-collision)
    (draw-rect player-rect blue :layer :debug-damage-collision)
    (when (rects-collide? rect player-rect)
      (replace-entity-state player (lambda (p) (player-take-damage p (damage-collision-amt (estate entity-id)))))
      (draw-rect rect magenta :layer :debug-damage-collision :filled? t)
      (draw-rect player-rect magenta :layer :debug-damage-collision :filled? t))))

(defstructure entity
    state
  subsystems
  system-type)

(let (entity-registry id)
  (defun current-entity-states ()
    (list id entity-registry))

  (defun restore-entity-states (id-and-registry)
    (mvsetq (id entity-registry) (values-list id-and-registry))
    (dohash (id e) entity-registry
      (register-entity-subsystems id e)))

  (defun init-id-system ()
    (setf id 0))
  (defun gen-entity-id ()
    (incf id))

  (defun init-entity-registry ()
    (setf id 0
	  entity-registry (make-hash-table)))

  (defun register-entity (id entity)
    (setf (gethash id entity-registry) entity)
    id)

  (defun estate (id)
    (entity-state (gethash id entity-registry)))

  (defun estate-set (id obj)
    (setf (entity-state (gethash id entity-registry)) obj)))

(defgeneric dead? (obj))
(defmethod dead? (obj)
  (declare (ignore obj))
  nil)

(defun register-entity-subsystems (id entity)
  (with-entity-slots (entity)
    (dolist (s subsystems)
      (ecase s
	(:ai (register-ai system-type id))
	(:bullet (register-bullet system-type id))
	(:damageable (register-damageable system-type id))
	(:damage-collision (register-damage-collision system-type id))
	(:input (register-input system-type id))
	(:physics (register-physics system-type id))
	(:stage-collision (register-stage-collision system-type id))
	(:dynamic-collision (register-dynamic-collision system-type id))
	(:drawable (register-drawable system-type id))
	(:pickup (register-pickup system-type id))))))

(defun create-entity (initial-state subsystems &key (id (gen-entity-id)))
  (let ((entity (make-entity :system-type entity-system-type :state initial-state :subsystems subsystems)))
    (register-entity-subsystems id entity)
    (register-entity id entity)))
