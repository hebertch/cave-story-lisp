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

(defvar *registry* nil
  "A registry table is a table of registry-name keyword to entity-ids.")
(defun registry-insert-id! (registry-key id)
  "Insert entity-id ID into the registry table."
  (setq *registry*
	(aupdate *registry* registry-key (pushfn id))))

(defun registry-update! (registry-key fn)
  (setq *registry*
	(aupdate *registry* registry-key fn)))
(defun registry-remove-dead! (registry-key)
  "Remove the dead entities associated with registry-key."
  (registry-update! registry-key
		    (lambda (ids)
		      (remove-if (lambda (id) (dead? (estate id))) ids))))

(defun registry-ids (registry-key)
  "Return a list of the ids in registry-key."
  (aval *registry* registry-key))
(defun clear-registry! ()
  "Clear the registry."
  (setq *registry* nil))

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

(defun update-physics-subsystem! ()
  (registry-remove-dead! :physics)
  (dolist (entity-id (registry-ids :physics))
    (update-world! entity-id #'physics)))

(defun update-timers-subsystem! ()
  (registry-remove-dead! :timers)
  (dolist (entity-id (registry-ids :timers))
    (update-world! entity-id #'timers)))

(defun ticked? (obj timer-key)
  (member timer-key (aval obj :ticks)))

(defun update-drawable-subsystem! ()
  (registry-remove-dead! :drawable)
  (dolist (entity-id (registry-ids :drawable))
    (let ((drawings (ensure-list (draw (estate entity-id)))))
      (appendf *render-list* drawings))))

(defun update-stage-collision-subsystem! (stage)
  (registry-remove-dead! :stage-collision)
  (dolist (entity-id (registry-ids :stage-collision))
    (update-world! entity-id
                   #_(stage-collision _ stage))))

(defun update-input-subsystem! (input)
  (registry-remove-dead! :input)
  (dolist (entity-id (registry-ids :input))
    (update-world! entity-id
                   #_(input _ input))))

(defun update-dynamic-collision-subsystem! (player)
  (registry-remove-dead! :dynamic-collision)
  (dolist (entity-id (registry-ids :dynamic-collision))
    (dolist (side *collision-order*)
      (let* ((state (estate entity-id))
             (rect (dynamic-collision-rect state))
             (player-collision-rect
              (cdr (assoc side *player-collision-rectangles-alist*)))
             (player-rect
              (rect-offset player-collision-rect
                           (physics-pos (estate player)))))
        (draw-rect! rect *blue* :layer :debug-dynamic-collision)
        (draw-rect! player-rect *green* :layer :debug-dynamic-collision)
        (when (rects-collide? rect player-rect)
          (draw-rect! player-rect *green* :layer :debug-dynamic-collision
                      :filled? t)
          (draw-rect! rect *yellow* :layer :debug-dynamic-collision :filled? t)
          (estate-set! player
                       (dynamic-collision-react state side
                                                player-collision-rect
                                                player)))))))

(defun update-damageable-subsystem! (bullet-id)
  (registry-remove-dead! :damageable)
  (dolist (entity-id (registry-ids :damageable))
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
          (update-world! bullet-id #'bullet-hit-react))))))

(defun update-bullet-subsystem! ()
  (registry-remove-dead! :bullet)
  (dolist (entity-id (registry-ids :bullet))
    (update-damageable-subsystem! entity-id)))

(defun update-pickup-subsystem! (player)
  (registry-remove-dead! :pickup)
  (dolist (entity-id (registry-ids :pickup))
    (let ((rect (pickup-rect (estate entity-id)))
          (player-rect (player-damage-collision-rect (estate player))))
      (draw-rect! rect *green* :layer :debug-pickup)
      (draw-rect! player-rect *blue* :layer :debug-pickup)
      (when (rects-collide? rect player-rect)
        (draw-rect! rect *yellow* :layer :debug-pickup :filled? t)
        (draw-rect! player-rect *yellow* :layer :debug-pickup :filled? t)
        (update-world! player
                       #_(player-pickup _ (estate entity-id)))
        (update-world! entity-id #'pickup-kill)))))

(defun update-damage-collision-subsystem! (player)
  (registry-remove-dead! :damage-collision)
  (dolist (entity-id (registry-ids :damage-collision))
    (let ((rect (damage-collision-rect (estate entity-id)))
          (player-rect (player-damage-collision-rect (estate player))))
      (draw-rect! rect *red* :layer :debug-damage-collision)
      (draw-rect! player-rect *blue* :layer :debug-damage-collision)
      (when (rects-collide? rect player-rect)
        (update-world! player
                       #_ (player-take-damage _
					      (damage-collision-amt
					       (estate entity-id))))
        (draw-rect! rect *magenta* :layer :debug-damage-collision :filled? t)
        (draw-rect! player-rect *magenta* :layer :debug-damage-collision
                    :filled? t)))))

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
  
  (defun init-id-system! ()
    (setq id 0))
  (defun gen-entity-id ()
    (setq id (1+ id))))

(defun init-entity-registry! ()
  (init-id-system!)
  (setq *current-entity-registry* (make-entity-registry)))

(defun estate (id &optional (er *current-entity-registry*))
  (let ((lookup (cdr (assoc id er))))
    lookup))
(defun estate-set! (id obj)
  (setq *current-entity-registry*
	(estate-set *current-entity-registry* id obj))
  :done)

(defun register-entity-subsystems! (id entity)
  (dolist (sys (aval entity :subsystems))
    (registry-insert-id! sys id))
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
