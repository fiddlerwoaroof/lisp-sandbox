(defpackage :lispostscript
  (:use :cl :alexandria :serapeum))
(in-package :lispostscript)

(defclass operator ()
  ())

(defclass environment ()
  ((&operators :initarg :operators :initform (make-hash-table))
   (&parent :initarg :parent :initform nil)))

(defgeneric op-stack-size (operator))

(defgeneric lookup-name (environment name)
  (:method-combination append))

(defmethod)

(defgeneric execute-op (op &rest args)
  )

(defun un-rpn (list environment)
  (loop with stack = (list)
     for token in list
     if (and (symbolp token)
	     (not (keywordp token)))
     do (let ((op (lookup-name environment token)))
	  (typecase op
	    (operator (let ((op-call (list)))
			(dotimes (n (op-stack-size op))
			  (push (pop stack) op-call))
			(push op stack)
			(push 'execute-op stack)))
	    (t (push op stack))))
     else do (push token stack)
     finally (return stack)))

