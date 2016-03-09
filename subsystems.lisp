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

(defun update-world (env id fn)
  (let ((obj (funcall fn (estate id (aval env :entity-registry)))))
    (aupdate (apply-effects env obj)
	     :entity-registry
	     (lambda (entity-registry)
	       (estate-set entity-registry id (estate id entity-registry))))))

(defun make-env ()
  (alist :entity-registry *current-entity-registry*
	 :registry *registry*))
(defun update-env! (env)
  (setq *current-entity-registry* (aval env :entity-registry)
	*registry* (aval env :registry)))

(defun update-subsystem (env key update-fn)
  (dolist (entity-id (aval *registry* key))
    (setq env (funcall update-fn env entity-id)))
  env)

(defun update-physics-entity (env id)
  (update-world env id #'physics))
(defun update-physics-entity! (id)
  (update-env! (update-physics-entity (make-env) id)))

(defun update-timers-entity (env id)
  (update-world env id #'timers))
(defun update-timers-entity! (id)
  (update-env! (update-timers-entity (make-env) id)))

(defun update-drawable-entity (env id)
  (let ((drawings (ensure-list (draw (estate id (aval env :entity-registry))))))
    (appendf *render-list* drawings))
  env)
(defun update-drawable-entity! (entity-id)
  (update-env! (update-drawable-entity (make-env) entity-id)))

(defun update-stage-collision-entity (env id)
  (let ((stage (estate (aval *global-game* :stage)
		       (aval env :entity-registry))))
    (update-world env id
		  #_(stage-collision _ stage))))
(defun update-stage-collision-entity! (entity-id)
  (update-env! (update-stage-collision-entity (make-env) entity-id)))

(defun update-input-entity (env id)
  (let ((input (aval *global-game* :input)))
    (update-world env id  #_(input _ input))))
(defun update-input-entity! (entity-id)
  (update-env! (update-input-entity (make-env) entity-id)))

(defun update-dynamic-collision-entity (env id)
  (let ((player (aval *global-game* :player)))
    (dolist (side *collision-order*)
      (let* ((state (estate id (aval env :entity-registry)))
	     (player-state
	      (estate player (aval env :entity-registry)))
	     (rect (dynamic-collision-rect state))
	     (player-collision-rect
	      (cdr (assoc side *player-collision-rectangles-alist*)))
	     (player-rect
	      (rect-offset player-collision-rect
			   (physics-pos player-state))))
	(draw-rect! rect *blue* :layer :debug-dynamic-collision)
	(draw-rect! player-rect *green* :layer :debug-dynamic-collision)
	(when (rects-collide? rect player-rect)
	  (draw-rect! player-rect *green* :layer :debug-dynamic-collision
		      :filled? t)
	  (draw-rect! rect *yellow* :layer :debug-dynamic-collision :filled? t)
	  (setq env
		(aupdate
		 env
		 :entity-registry
		 #_(estate-set
		    _
		    player
		    (dynamic-collision-react state side
					     player-collision-rect
					     player))))))))
  env)
(defun update-dynamic-collision-entity! (entity-id)
  (update-env! (update-dynamic-collision-entity (make-env) entity-id)))

(defun update-pickup-entity (env id)
  (let ((player-id (aval *global-game* :player)))
    (let* ((state (estate id (aval env :entity-registry)))
	   (rect (pickup-rect state))
	   (player-rect (player-damage-collision-rect
			 (estate player-id (aval env :entity-registry)))))
      (draw-rect! rect *green* :layer :debug-pickup)
      (draw-rect! player-rect *blue* :layer :debug-pickup)
      (when (rects-collide? rect player-rect)
	(draw-rect! rect *yellow* :layer :debug-pickup :filled? t)
	(draw-rect! player-rect *yellow* :layer :debug-pickup :filled? t)
	(setq env (update-world env player-id #_(player-pickup _ state)))
	(setq env (update-world env id #'pickup-kill)))))
  env)
(defun update-pickup-entity! (entity-id)
  (update-env! (update-pickup-entity (make-env) entity-id)))

(defun update-damage-collision-entity (env id)
  (let ((player-id (aval *global-game* :player)))
    (let ((rect (damage-collision-rect
		 (estate id (aval env :entity-registry))))
	  (player-rect (player-damage-collision-rect
			(estate player-id (aval env :entity-registry)))))
      (draw-rect! rect *red* :layer :debug-damage-collision)
      (draw-rect! player-rect *blue* :layer :debug-damage-collision)
      (when (rects-collide? rect player-rect)
	(setq env (update-world env
				player-id
				#_ (player-take-damage
				    _
				    (damage-collision-amt
				     (estate id (aval env :entity-registry))))))
	(draw-rect! rect *magenta* :layer :debug-damage-collision :filled? t)
	(draw-rect! player-rect *magenta* :layer :debug-damage-collision
		    :filled? t))))
  env)
(defun update-damage-collision-entity! (entity-id)
  (update-env! (update-damage-collision-entity (make-env) entity-id)))


(defun update-damageable-subsystem (env bullet-id)
  (update-subsystem
   env
   :damageable
   (lambda (env id)
     (unless (dead? (estate bullet-id (aval env :entity-registry)))
       (let ((bullet-rect
	      (bullet-rect (estate bullet-id (aval env :entity-registry))))
	     (bullet-hit-amt
	      (bullet-damage-amt (estate bullet-id (aval env :entity-registry))))
	     (rect
	      (damageable-rect (estate id (aval env :entity-registry)))))
	 (draw-rect! bullet-rect *green* :layer :debug-damageable)
	 (draw-rect! rect *blue* :layer :debug-damageable)
	 (when (rects-collide? rect bullet-rect)
	   (draw-rect! bullet-rect *yellow* :layer :debug-damageable :filled? t)
	   (draw-rect! rect *yellow* :layer :debug-damageable :filled? t)
	   (setq env
		 (update-world env id
			       #_(damageable-hit-react _ bullet-hit-amt)))
	   (setq env
		 (update-world env bullet-id #'bullet-hit-react)))))
     env)))
(defun update-damageable-subsystem! (bullet-id)
  (update-env! (update-damageable-subsystem (make-env) bullet-id)))

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
