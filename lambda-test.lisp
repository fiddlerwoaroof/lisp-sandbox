(defpackage :lambda-test
  (:use cl)
  (:export :mksym :alambda))
(in-package :lambda-test)

(defun mksym (str)
  (intern (string-upcase str) *package*))

(defmacro alambda ((&optional (nargs 1)) &body body)
  (let ((args (loop for x from 1 to nargs collect (mksym (format nil "$~a" x)))))
    `(lambda ,args ,@body)))

(defmacro /. ((&optional (nargs 1)) &body body)
  `(alambda (,nargs) ,@body))
