(defpackage :fwoar.docstrings
  (:use :cl )
  (:export ))
(in-package :fwoar.docstrings)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun docstring (name &key signature return summary examples doc)
    (format nil "(~s ~{~s~^ ~}) => ~s ~a~%~%~:[~;~:*~a~%~%~]~{(~/pprint-linear/)~%~}"
            name signature return summary
            doc
            examples)))

(defmacro document (definition (=> return) &body (doc &rest examples))
  (assert (eql => '=>))
  (ecase (car definition)
    (defun (destructuring-bind (_ name arguments summary &rest __) definition
             (declare (ignore _ __))
             `(progn ,definition
                     (setf (documentation ',name 'function)
                           ,(docstring name :signature arguments :return return :summary summary :examples examples :doc doc)))))))

(document
    (defun this-is-a-test (a b c)
      "this is a test"
      (declare (ignore a b c))
      (values))
    (=> values)
  "don't do nothin"
  (this-is-a-test 1 2 3)
  (this-is-a-test 'a 2 3))
