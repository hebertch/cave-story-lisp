;;;; cave-story.asd

(asdf:defsystem #:cave-story
  :description "Describe cave-story here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:swank-tools
	       #:sdl)
  :serial t
  :components ((:file "package")
	       (:file "math")
	       (:file "parameters")
	       (:file "subsystems")
	       (:file "resources")
	       (:file "timers")
	       (:file "renderer")
	       (:file "input")
	       (:file "physics")
	       (:file "collisions")
	       (:file "stage")
	       (:file "player")
	       (:file "camera")
	       (:file "guns")
	       (:file "projectiles")
	       (:file "cave-story")))
