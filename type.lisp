(defpackage :the-type
  (:use :cl))
(in-package :the-type)

(defclass environment ()
  ((%vars :initarg :vars :initform (make-hash-table))
   (%)))

(dolist (item list result)
  item)

(defun format-dolist (dolist-form)
  kj)
