;;;; package.lisp

(defpackage #:cave-story
  (:use #:cl #:utilities))

;; Reload the lisp system before changing this!
(setf utilities:*defstructure-use-struct* nil)
