(defpackage :curly-bracens
  (:use :cl :named-readtables))
(in-package :curly-bracens)
(shadow 'if)

(defmacro if (condition then &optional (else-sym nil e-s-p) (else nil e-p))
  (assert (and e-s-p e-p (eq else-sym 'else)))
  `(cl:if ,condition
          ,then
          ,else))

(defun read-progn (stream char)
  (declare (ignore char))
  (cons 'progn
        (read-delimited-list #\} stream t)))

(defreadtable :curly-bracens
  (:merge :standard)
  (:macro-char #\} (lambda (&rest r) (declare (ignore r))) nil)
  (:macro-char #\{ 'read-progn nil))

(if (> 3 1) {
  (princ :hi)
  (terpri)
  (+ 2 3)
} else {
  (princ :bye)
  (terpri)
  (+ 4 5)
})
