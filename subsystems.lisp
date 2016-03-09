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
(defun registry-insert-id (table registry-key id)
  "Insert entity-id ID into the registry table."
  (aupdate table registry-key (pushfn id)))

(defun registry-remove-dead (registry)
  "Remove the dead entities associated with all registry-keys."
  (mapcar (lambda (key-and-ids)
	    (cons (car key-and-ids)
		  (remove-if (lambda (id) (dead? (estate id))) (cdr key-and-ids))))
	  registry))

(defun update-world! (entity-id fn)
  (let ((obj (funcall fn (estate entity-id)))
	(env (alist :entity-registry *current-entity-registry*
		    :registry *registry*)))
    (let ((env2 (apply-effects env obj)))
      (setq *current-entity-registry* (aval env2 :entity-registry)
	    *registry* (aval env2 :registry)))
    (setq *current-entity-registry*
	  (estate-set *current-entity-registry* entity-id (estate entity-id)))))

(defun update-subsystem (key update-fn)
  (dolist (entity-id (aval *registry* key))
    (funcall update-fn entity-id)))

(setfn update-physics-entity! #_(update-world! _ #'physics))
(setfn update-timers-entity! #_(update-world! _ #'timers))
(defun update-drawable-entity! (entity-id)
  (let ((drawings (ensure-list (draw (estate entity-id)))))
    (appendf *render-list* drawings)))
(defun update-stage-collision-entity! (entity-id)
  (update-world! entity-id
		 #_(stage-collision _ (estate (aval *global-game* :stage)))))
(defun update-input-entity! (entity-id)
  (update-world! entity-id #_(input _ (aval *global-game* :input))))
(defun update-dynamic-collision-entity! (entity-id)
  (let ((player (aval *global-game* :player)))
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
	  (setq *current-entity-registry*
		(estate-set
		 *current-entity-registry*
		 player
		 (dynamic-collision-react state side
					  player-collision-rect
					  player))))))))
(defun update-pickup-entity! (entity-id)
  (let ((player (aval *global-game* :player)))
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
(defun update-damage-collision-entity! (entity-id)
  (let ((player (aval *global-game* :player)))
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

(defun update-physics-subsystem! ()
  (update-subsystem :physics #'update-physics-entity!))
(defun update-timers-subsystem! ()
  (update-subsystem :timers #'update-timers-entity!))
(defun update-drawable-subsystem! ()
  (update-subsystem :drawable #'update-drawable-entity!))
(defun update-stage-collision-subsystem! ()
  (update-subsystem :stage-collision #'update-stage-collision-entity!))
(defun update-input-subsystem! ()
  (update-subsystem :input #'update-input-entity!))
(defun update-dynamic-collision-subsystem! ()
  (update-subsystem :dynamic-collision #'update-dynamic-collision-entity!))
(defun update-bullet-subsystem! ()
  (update-subsystem :bullet #'update-damageable-subsystem!))
(defun update-pickup-subsystem! ()
  (update-subsystem :pickup #'update-pickup-entity!))
(defun update-damage-collision-subsystem! ()
  (update-subsystem :damage-collision #'update-damage-collision-entity!))

(defun update-damageable-subsystem! (bullet-id)
  (update-subsystem
   :damageable
   (lambda (entity-id)
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
	   (update-world! bullet-id #'bullet-hit-react)))))))

(defun ticked? (obj timer-key)
  (member timer-key (aval obj :ticks)))

(defvar *current-entity-registry* nil
  "A mapping of entity-id -> current state.")

(let (id)
  (defun current-entity-states ()
    (list id *current-entity-registry*))

  (defun restore-entity-states! (id-and-registry)
    (setq id (first id-and-registry))
    (setq *current-entity-registry* (second id-and-registry))
    (loop for (id . e) in *current-entity-registry*
       do
	 (setq *registry* (register-entity-subsystems *registry* id e))))
  
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

(defun register-entity-subsystems (registry id entity)
  (dolist (sys (aval entity :subsystems))
    (setq registry (registry-insert-id registry sys id)))
  registry)


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

(defun apply-effects (env obj)
  (appendf *sfx-play-list* (aval obj :sound-effects))
  
  (loop for state in (aval obj :new-states) do
       (setq env
	     (if (estate (aval state :id))
		 (aupdate env :entity-registry
			  #_(estate-set _ (aval state :id) state))
		 (create-entity env (aval state :id) state))))
  (aupdate env
	   :entity-registry
	   #_(estate-set _
			 (aval obj :id)
			 (arem obj
			       :sound-effects
			       :new-states))))

(defun create-entity (env id initial-state)
  (funcall
   (comp (lambda (env)
	   (aupdate env
		    :registry
		    #_(register-entity-subsystems
		       _ id (estate id (aval env :entity-registry)))))
	 #_(apply-effects _ initial-state))
   env))
