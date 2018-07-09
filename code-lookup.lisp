(defpackage :code-lookup
  (:use :cl :serapeum :alexandria :fw.lu)
  (:export ))
(in-package :code-lookup)


(defun call-with-auto-coercions (coercions cb)
  (handler-bind ((type-error (lambda (c)
                               (loop for ((from-type to-type) . coercion) in coercions
                                  when (and (subtypep to-type (type-error-expected-type c))
                                            (subtypep (type-of (type-error-datum c)) from-type))
                                  do (store-value (funcall coercion (type-error-datum c)))))))
    (funcall cb)))
