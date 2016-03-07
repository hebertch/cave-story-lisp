;;;; load.lisp

(in-package :cl-user)

#+nil
(define-key chebert-slime-mode-map "a"
  (lambda ()
    (interactive)
    (let ((slime-buffer-package 'cl-user))
      (slime-interactive-eval "(load-cave-story-system!)"))))

(defparameter *system-dependencies*
  '(:swank-tools
    :sdl
    :split-sequence)
  "External dependencies for the cave-story system.")

(defparameter *system-file-names*
  '("package"
    "math"
    "parameters"
    "subsystems"
    "resources"
    "timers"
    "renderer"
    "input"
    "physics"
    "collisions"
    "stage"
    "player"
    "camera"
    "guns"
    "projectiles"
    "cave-story")
  "Ordered list of file names that the cave story system loads.")

(defun load-cave-story-system! ()
  "Load the cave-story system. Deletes the cave story package if it exists."
  (ql:quickload *system-dependencies*)
  (when (find-package :cave-story)
    (delete-package :cave-story))
  (mapc #'load (mapcar (lambda (n) (concatenate 'string n ".lisp")) *system-file-names*))
  :cave-story)

(load-cave-story-system!)

(defmacro reload-cave-story ()
  '(progn
    (in-package :cl-user)
    (load-cave-story-system!)
    (in-package :cave-story)))

(defun group-by (key list)
  "Group list into lists sharing the same key.
Key is a function. 
Does not modify the order of the list.
Returns a list of (key . sublist) pairs."
  (loop until (null list)
     collecting
       (let ((key-val (funcall key (car list))))
	 (cons key-val
	       (loop
		  while (and list
			     (eq (funcall key (car list)) key-val))
		  collecting (car list)
		  do (setq list (cdr list)))))))

(defun group-by-package (imports)
  (mapcar (lambda (g) (cons (make-symbol (package-name (car g)))
			    (mapcar (lambda (s) (make-symbol (symbol-name s)))
				    (cdr g))))
	  (group-by #'symbol-package imports)))

(defun imported-symbols (package)
  "Return a list of symbols explicitly imported, not used or shadowing-imported,
by package."
  (let ((syms))
    (do-symbols (sym package)
      (when (not (member (symbol-package sym)
			 (cons package (package-use-list package))))
	(push sym syms)))
    syms))

(defun exported-symbols (package)
  "Return a list of symbols exported by package."
  (let ((syms))
    (do-external-symbols (sym package)
      (push (make-symbol (symbol-name sym)) syms))
    syms))

(defun shadowing-imports (package)
  "Return a list of symbols that are shadowing imports 
 (shadowing-import-from in defpackage)."
  (let* ((shadow-symbols (package-shadowing-symbols package))
	 (shadow-imports (remove-if (lambda (sym) (eq (symbol-package sym) package)) shadow-symbols)))
    (mapcar (lambda (g) (cons :shadowing-import-from g))
	    (group-by-package shadow-imports))))

(defun shadowing-symbols (package)
  "Return a list of symbols in this package that shadow
imported symbols."
  (let ((shadow-symbols (package-shadowing-symbols package)))
    (remove-if-not (lambda (sym) (eq (symbol-package sym) package)) shadow-symbols)))

(defun defpackage-form (package)
  "Returns what would be the form for a defpackage of package.
Does not take into account things like reader-macros."
  (let* ((package (find-package package))
	 (package-name (make-symbol (package-name package)))
	 (use (mapcar (lambda (p) (make-symbol (package-name p))) (package-use-list package)))
	 (import-froms (mapcar (lambda (g) (cons :import-from g)) (group-by-package (imported-symbols package))))
	 (exports (exported-symbols package))
	 (shadowing-import-froms (mapcar (lambda (g) (cons :shadowing-import-from g))
					 (shadowing-imports package)))
	 (shadows (shadowing-symbols package))
	 (nicknames (mapcar (lambda (s) (make-symbol s)) (package-nicknames package))))
    (remove nil
	    (append
	     (list 'defpackage package-name
		   (when use (cons :use use))
		   (when shadows (cons :shadow shadows))
		   (when exports (cons :export exports))
		   (when nicknames (cons :nicknames nicknames)))
	     shadowing-import-froms
	     import-froms))))