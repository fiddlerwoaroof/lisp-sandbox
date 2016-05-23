(ql:quickload :alexandria)

(defpackage generator-conditions
  (:use :cl))
(in-package generator-conditions)

(define-condition generator-condition () ())
(define-condition yield-value (generator-condition)
  ((value :initarg :value :reader value)))
(define-condition stop (generator-condition) ())

(defmacro define-generator (name args &body body)
  `(flet ((yield (v) (signal (make-condition 'yield-value :value v)))
          (stop () (signal (make-condition 'stop))))
     (defun ,name ,args
       ,@body)))

(defmacro with-generator (generator var &body body)
  `(handler-case
     (handler-bind
       ((yield-value (lambda (c)
                       (let ((,var (value c)))
                         ,@body))))
       ,generator)
     (stop (c) (declare (ignore c)))))

(define-generator to-ten ()
  (loop for x from 0 to 10
        do (yield x)))

(defparameter outp nil)
(with-generator (to-ten) x
  (with-generator (to-ten) y
    (push (* x y) outp)))

(defpackage generator-lambda
  (:use :cl))
(in-package generator-lambda)



