;;;; cave-story.asd

(asdf:defsystem #:cave-story
  :description "Describe cave-story here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:utilities
               #:swank-tools
               #:sdl)
  :serial t
  :components ((:file "package")
	       (:file "math")
	       (:file "parameters")
	       (:file "image-assets")
	       (:file "sfx")
	       (:file "renderer")
	       (:file "input")
	       (:file "physics")
	       (:file "collisions")
	       (:file "stage")
	       (:file "player")
	       (:file "guns")
	       (:file "projectiles")
               (:file "cave-story")))

