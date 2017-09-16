(defpackage :patmatch
  (:use :cl :alexandria :serapeum)
  (:export pat-match))
(in-package :patmatch)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric handle-pattern (pattern form &rest args)
    (:method ((pattern cons) form &rest args)
      (let ((val-sym (gensym "VAL")))
        (destructuring-bind (car cdr) args
          `((,val-sym ,form)
            (,car (car ,val-sym))
            (,cdr (cdr ,val-sym))))))

    (:method ((pattern vector) form &rest args)
      (let ((val-sym (gensym "VAL")))
        `((,val-sym ,form)
          ,@ (loop for arg in args
                for idx from 0
                collect `(,arg (aref ,val-sym ,idx))))))

    (:method ((pattern hash-table) form &rest args)
      (let* ((val-sym (gensym "VAL"))
             (binding-forms (loop for (key sym) in args
                               append `((,sym (gethash ',key ,val-sym))))))
        `((,val-sym ,form)
          ,@binding-forms)))

    (:method ((pattern symbol) form &rest args)
      (apply #'handle-pattern
             (closer-mop:class-prototype
              (find-class pattern))
             form
             args)))) 

(defmacro pattern-match ((&rest clauses) &body body)
  `(let* (,@ (loop for ((discriminator . args) val-form) in clauses
                append (apply 'handle-pattern discriminator val-form args)))
     ,@body))



(pattern-match (((hash-table (:a a) (:b b)) (plist-hash-table '(:a 1 :b 2))))
  (+ a b))
