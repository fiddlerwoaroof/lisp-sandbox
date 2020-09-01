(cl:in-package :cl-user)
(defpackage :fwoar.smart-quote-readtable
  (:use :cl)
  (:export #:standard+smart-quotes
           #:smart-quotes))
(in-package :fwoar.smart-quote-readtable)
(declaim (special _ __))

(defmacro define-smart-quote-reader (name ending-quote)
  `(defun ,name (s _)
     (coerce (loop for c = (read-char s t nil t)
                   until (char= c ,ending-quote)
                   collect c)
             'string)))

(define-smart-quote-reader read-smart-double-quote #\”)
(define-smart-quote-reader read-smart-single-quote #\’)

(defun error-unmatched-closing-quote (_ __)
  (error "unmatched closing smart quote"))

(named-readtables:defreadtable smart-quotes
  (:macro-char #\“ 'read-smart-double-quote)
  (:macro-char #\” 'error-unmatched-closing-quote)
  (:macro-char #\‘ 'read-smart-single-quote)
  (:macro-char #\’ 'error-unmatched-closing-quote))

(named-readtables:defreadtable standard+smart-quotes
  (:merge :standard)
  (:fuse smart-quotes))

(defvar *doc-table* (make-hash-table))
(defmethod documentation ((object symbol) (doc-type (eql 'readtable)))
  (gethash object *doc-table*))
(defmethod (setf documentation) (newdoc (object symbol) (doc-type (eql 'readtable)))
  (setf (gethash object *doc-table*)
        newdoc))
