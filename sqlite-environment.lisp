(defpackage :sqlite-environment
  (:use :cl )
  (:export ))
(in-package :sqlite-environment)

(defparameter *sqlite-env* (list))

(defmacro defun* (name (&rest args) &body body)
  `(progn
     (push '(list ,name ,args ,body)
           *sqlite-env*)
     (defun ,name ,args
       ,@body)))

(defun* do-thing ()
  (asdf))
