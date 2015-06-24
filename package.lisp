;;;; package.lisp

(defpackage #:cave-story
  (:use #:cl #:utilities #:alist-structure))

;; Reload the lisp system before changing this!
(setf utilities:*defstructure-use-struct* nil)
