(defpackage :the-type
  (:use :cl))
(in-package :the-type)

(defclass binding ()
  ((%type :initarg :type :accessor type-binding)
   (%value :initarg :value :accessor value-binding)))

(defclass environment ()
  ((%vars :initarg :vars :initform (make-hash-table))
   (%funcs :initarg :funcs :initform (make-hash-table))))

(defparameter *the-env* (make-instance 'environment))

(defmacro define-function-binding (name env (&key return-type) (&rest args) &body body)
  `(setf (gethash ',name (funcs ,env)
                  )))

(progn
  (setf (gethash '+ (funcs *the-env*))
        #'+)
  (setf (gethash '+ (funcs *the-env*))
        #'+)
  )

(defun annotate-function-call (function-type call-form)
  (mapcar (lambda (type-element call-element)
            (list type-element call-element))
          function-type call-form))

(defun check-function-type (function args)
  (let ((type-sig (case function
                    ('+ '(number number))
                    ('* '(number number))
                    ('length '(string)))))
    (when (= (length type-sig) (length args))
      )))
(defun check-types (form)
  (typecase form
    (cons (check-function-type (car form) (cdr form)))
    (t t)))
