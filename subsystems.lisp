(in-package :cave-story)

(defun motion-physics (obj)
  (let ((fn (aval obj :physics-fn)))
    (if fn
	(funcall fn obj)
	obj)))
(defun motion-pos (obj)
  (let ((fn (aval obj :pos-fn)))
    (if fn
	(funcall fn obj)
	obj)))
(defun origin (obj)
  (funcall (aval obj :origin-fn) obj))
(defun inertia-vel (obj)
  (funcall (aval obj :inertia-vel-fn) obj))
(defun ai (obj)
  "Call the objects :ai-fn on obj if it has one."
  (let ((fn (aval obj :ai-fn)))
    (if fn
	(funcall fn obj)
	obj)))

(defun draw (obj)
  (funcall (aval obj :draw-fn) obj))
(defun stage-collision (obj stage)
  (funcall (aval obj :stage-collision-fn) obj stage))
(defun input (obj input)
  (funcall (aval obj :input-fn) obj input))
(defun dynamic-collision-react (obj side player-collision-rect player)
  (funcall (aval obj :dynamic-collision-react-fn) obj side player-collision-rect player))
(defun dynamic-collision-rect (obj)
  (funcall (aval obj :dynamic-collision-rect-fn) obj))
(defun damageable-rect (obj)
  (funcall (aval obj :damageable-rect-fn) obj))
(defun damageable-hit-react (obj bullet-hit-amt)
  "Calls the obj's :damageable-hit-react-fn.
Binds :damage-amt (in obj) to the bullet hit amount."
  (funcall (aval obj :damageable-hit-react-fn)
	   (aset obj :damage-amt bullet-hit-amt)))

(defun bullet-rect (obj)
  (funcall (aval obj :bullet-rect-fn) obj))
(defun bullet-hit-react (obj)
  (funcall (aval obj :bullet-hit-react-fn) obj))
(defun bullet-damage-amt (obj)
  (funcall (aval obj :bullet-damage-amt-fn) obj))

(defun pickup-rect (obj)
  (funcall (aval obj :pickup-rect-fn) obj))

(defun pickup-data (obj)
  (funcall (aval obj :pickup-data-fn) obj))

(defun damage-collision-rect (obj)
  (funcall (aval obj :damage-collision-rect-fn) obj))
(defun damage-collision-amt (obj)
  (funcall (aval obj :damage-collision-amt-fn) obj))

(defun dead? (obj)
  (aval obj :dead?))

(defun update-timer (tr)
  (funcall (aval tr :update-fn) tr))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *registry-syms* nil)

  (defun interface-forms-name (interface)
    (symbolicate interface '-forms))

  (defun lambda-form (interface)
    `(list* 'lambda (list ,@ (cdr interface))
	    ,(interface-forms-name (car interface)))))

(defmacro def-subsystem (name update-args &body update-forms)
  "Creates a subsytem of NAME.
REGISTER-name is the interface to add to name-REGISTRY.
DEF-ENTITY-name is the interface to be used with DEF-ENTITY.
UPDATE-name-SUBSYSTEM evaluates UPDATE-FORMS given INTERFACE and UPDATE-ARGS."

  (let ((registry (symbolicate '* name '-registry*))
	(register-name (symbolicate 'register- name '!))
	(update-name (symbolicate 'update- name '-subsystem!)))
    (with-gensyms (entry-name)
      `(progn
	 (defvar ,registry nil)
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (pushnew ',registry *registry-syms*))

	 (defun ,register-name (system-type id)
	   (push (cons system-type id) ,registry))

	 (defun ,update-name
	     ,(cons 'active-entity-systems update-args)
	   (setq ,registry (remove-if (lambda (p)
					(dead? (estate (cdr p))))
				      ,registry))
	   (dolist (,entry-name ,registry)
	     (when (member (car ,entry-name) active-entity-systems)
	       (let ((entity-id (cdr ,entry-name)))
		 ,@update-forms))))

	 (values ',registry ',register-name ',update-name)))))

(defun apply-effects! (entity-registry obj)
  (appendf *sfx-play-list* (aval obj :sound-effects))
  
  (loop for state in (aval obj :new-states) do
       (if (estate (aval state :id))
	   (setq entity-registry (estate-set entity-registry (aval state :id) state))
	   (setq entity-registry (create-entity! entity-registry (aval state :id) state))))
  (estate-set entity-registry
	      (aval obj :id)
	      (arem obj
		    :sound-effects
		    :new-states)))

(defun update-world! (entity-id fn)
  (let ((obj (funcall fn (estate entity-id))))
    (setq *current-entity-registry* (apply-effects! *current-entity-registry* obj))
    (estate-set! entity-id (estate entity-id))))

(def-subsystem physics ()
  (update-world! entity-id #'physics))

(def-subsystem timers ()
  (update-world! entity-id #'timers))

(defun ticked? (obj timer-key)
  (member timer-key (aval obj :ticks)))

(def-subsystem drawable ()
  (let ((drawings
	 (alexandria:ensure-list (draw (estate entity-id)))))
    (appendf *render-list* drawings)))
(def-subsystem stage-collision (stage)
  (update-world! entity-id #_(stage-collision _ stage)))
(def-subsystem input (input)
  (update-world! entity-id #_(input _ input)))

(def-subsystem dynamic-collision (player)
  (dolist (side *collision-order*)
    (let* ((state (estate entity-id))
	   (rect (dynamic-collision-rect state))
	   (player-collision-rect
	    (cdr (assoc side *player-collision-rectangles-alist*)))
	   (player-rect
	    (rect-offset player-collision-rect (physics-pos (estate player)))))
      (draw-rect! rect *blue* :layer :debug-dynamic-collision)
      (draw-rect! player-rect *green* :layer :debug-dynamic-collision)
      (when (rects-collide? rect player-rect)
	(draw-rect! player-rect *green* :layer :debug-dynamic-collision :filled? t)
	(draw-rect! rect *yellow* :layer :debug-dynamic-collision :filled? t)
	(estate-set! player (dynamic-collision-react state
						     side
						     player-collision-rect
						     player))))))

(def-subsystem damageable (bullet-id)
  ;; NOTE: UPDATE-DAMAGEABLE-SUBSYSTEM is designed to be called
  ;; by UPDATE-BULLET-SUBSYSTEM
  (unless (dead? (estate bullet-id))
    (let ((bullet-rect (bullet-rect (estate bullet-id)))
	  (bullet-hit-amt (bullet-damage-amt (estate bullet-id)))
	  (rect (damageable-rect (estate entity-id))))
      (draw-rect! bullet-rect *green* :layer :debug-damageable)
      (draw-rect! rect *blue* :layer :debug-damageable)
      (when (rects-collide? rect bullet-rect)
	(draw-rect! bullet-rect *yellow* :layer :debug-damageable :filled? t)
	(draw-rect! rect *yellow* :layer :debug-damageable :filled? t)
	(update-world! entity-id
		       #_(damageable-hit-react _ bullet-hit-amt))
	(update-world! bullet-id #'bullet-hit-react)))))

(def-subsystem bullet ()
  (update-damageable-subsystem! active-entity-systems entity-id))

(def-subsystem pickup (player)
  (let ((rect (pickup-rect (estate entity-id)))
	(player-rect  (player-damage-collision-rect (estate player))))
    (draw-rect! rect *green* :layer :debug-pickup)
    (draw-rect! player-rect *blue* :layer :debug-pickup)
    (when (rects-collide? rect player-rect)
      (draw-rect! rect *yellow* :layer :debug-pickup :filled? t)
      (draw-rect! player-rect *yellow* :layer :debug-pickup :filled? t)
      (update-world! player #_(player-pickup _ (estate entity-id)))
      (update-world! entity-id #'pickup-kill))))

(def-subsystem damage-collision (player)
  (let ((rect (damage-collision-rect (estate entity-id)))
	(player-rect (player-damage-collision-rect (estate player))))
    (draw-rect! rect *red* :layer :debug-damage-collision)
    (draw-rect! player-rect *blue* :layer :debug-damage-collision)
    (when (rects-collide? rect player-rect)
      (update-world!
       player
       #_(player-take-damage _ (damage-collision-amt (estate entity-id))))
      (draw-rect! rect *magenta* :layer :debug-damage-collision :filled? t)
      (draw-rect! player-rect
		  *magenta*
		  :layer :debug-damage-collision
		  :filled? t))))

;; TODO: Make this functional...
;; Pass/return the entity-registry/id pair as an entity-registry-system.
;; Register-entity! should be register-entity and return a new entity-registry-system
(defvar *current-entity-registry*)

(let (id)
  (defun current-entity-states ()
    (list id *current-entity-registry*))

  (defun restore-entity-states! (id-and-registry)
    (setq id (first id-and-registry))
    (setq *current-entity-registry* (second id-and-registry))
    (loop for (id . e) in *current-entity-registry*
       do
	 (register-entity-subsystems! id e)))
  
  (defun init-id-system ()
    (setq id 0))
  (defun gen-entity-id ()
    (setq id (1+ id)))

  (defun estate (id &optional (er *current-entity-registry*))
    (let ((lookup (cdr (assoc id er))))
      lookup))

  (defun init-entity-registry! ()
    (setq id 0
	  *current-entity-registry* (make-entity-registry)))

  (defun estate-set! (id obj)
    (setq *current-entity-registry*
	  (estate-set *current-entity-registry* id obj))
    :done))

(defun register-entity-subsystems! (id entity)
  (let ((system-type :game))
    (dolist (s (aval entity :subsystems))
      (ecase s
	((:ai :timers) (register-timers! system-type id))
	(:bullet (register-bullet! system-type id))
	(:damageable (register-damageable! system-type id))
	(:damage-collision (register-damage-collision! system-type id))
	(:input (register-input! system-type id))
	(:physics (register-physics! system-type id))
	(:stage-collision (register-stage-collision! system-type id))
	(:dynamic-collision (register-dynamic-collision! system-type id))
	(:drawable (register-drawable! system-type id))
	(:pickup (register-pickup! system-type id)))))
  :done)

(defun create-entity! (entity-registry id initial-state)
  (setq entity-registry (apply-effects! entity-registry initial-state))
  (register-entity-subsystems! id (estate id entity-registry))
  (estate-set entity-registry id (estate id entity-registry)))

(defun make-entity-registry () nil)

(defun estate-set (entity-registry id state)
  "Return an entity-registry with the entity identified
by id updated to have state."
  (aset entity-registry id state))

(setfn physics motion-set-update)
(setfn physics-pos motion-set-pos)
(setfn timers
       "Return o with its :timers updated :ticks set, and ai applied."
       ai timer-set-update (asetfn :ticks nil))
