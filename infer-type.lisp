(defpackage :infer-type
  (:use :cl :serapeum)
  (:export ))
(in-package :infer-type)

(defclass type-env ()
  ((%typedefs :initform (make-hash-table)
              :initarg :typedefs
              :reader typedefs)
   (%typebindings :initform (make-hash-table)
                  :initarg :type-bindings
                  :reader typebindings)
   (%values :initform (make-hash-table)
            :reader values-)))

(defparameter *type-env* (make-instance 'type-env))

(defmacro with-types ((&rest types) (&rest defs) &body body)
  `(let ((*type-env* (make-instance 'type-env
                                    :typedefs
                                    (alexandria:alist-hash-table ',(loop for (type def) in types
                                                                      collect (cons type def)))
                                    :type-bindings
                                    (alexandria:alist-hash-table ',defs))))
     ,@body))

(defun lookup-type (type)
  (gethash type (typedefs *type-env*)))

(defun get-function-type (expr)
  (gethash (car expr)
           (typebindings *type-env*)))

(defun is-type-variable (tyvar)
  (and (symbolp tyvar)
       (char= (elt (symbol-name tyvar)
                   0)
              #\?)))

(defun is-bound-type-variable (tyvar)
  (nth-value 1 (gethash tyvar (typebindings *type-env*))))

(defun lookup-type-variable (tyvar)
  (gethash tyvar (typebindings *type-env*)))

(defun infer-simple-type (expr)
  (loop for k being the hash-keys of (typebindings *type-env*)
     using (hash-value v)
     when (subtypep (type-of expr) v) do
       (return k)))

(defun infer-complex-type (expr)
  (destructuring-bind (ret-type arg-types) (get-function-type expr)
    ))

(defun infer-type (expr)
  (if (is-simple-expression expr)
      (infer-simple-type expr)
      (infer-complex-type expr)))

(defun bind-type-variable (expr tyvar)
  (setf (gethash tyvar (typebindings *type-env*))
        (infer-type expr)))

(defun check-type- (expr type)
  (if (is-type-variable type)
      (if (is-bound-type-variable type)
          (check-type- expr
                       (lookup-type-variable type))
          (bind-type-variable expr type))
      (if (is-simple-expression expr)
          (check-simple-type expr type)
          (check-complex-type expr type))))

(defun is-simple-expression (expr)
  (or (not (listp expr))
      (eql (car expr)
           'quote)))

(defun check-simple-type (expr type)
  (values (subtypep (type-of expr)
                    (lookup-type type))
          type))

(defun check-complex-type (expr type)
  (destructuring-bind (return-type arg-types) (get-function-type expr)
    (values (and (lookup-type return-type)
                 (subtypep (lookup-type return-type)
                           (lookup-type type))
                 (= (1- (length expr))
                    (length arg-types))
                 (every (lambda (arg-expr arg-type)
                          (and (lookup-type arg-type)
                               (check-type- arg-expr arg-type)))
                        (cdr expr)
                        arg-types))
            return-type)))


(defun foo (a b)
  (check-type a number)
  (check-type b number)
  (+ a b))

#+null
(defmacro with-implicit-conversions ((&rest conversions) &body body)
  `(let ((num-coercions 0)
         (coercion-limit 10))
     (handler-bind ((type-error (lambda (c)
                                  (incf num-coercions)
                                  (when (< num-coercions
                                           coercion-limit)
                                    (let ((datum (type-error-datum c))
                                          (expected-type (type-error-expected-type c)))
                                      (store-value
                                       (cond ,@(loop for (type handler) in conversions
                                                  collect `((subtypep ',type expected-type)
                                                            (,handler datum))))))))))
       ,@body)))
