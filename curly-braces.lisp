(defpackage :curly-bracens
  (:use :cl :named-readtables)
  (:shadow :if :function :return))
(in-package :curly-bracens)

(defmacro if (condition then &optional (else-sym nil e-s-p) (else nil e-p))
  (assert (and e-s-p e-p (eq else-sym 'else)))
  `(cl:if ,condition
          ,then
          ,else))

(defmacro function (&body info)
  (cl:if (symbolp (car info))
      (destructuring-bind (name args &rest body) info
         `(defun ,name ,args
            ,@body))
      (destructuring-bind (args &rest body) info
        `(lambda ,args
           ,@body))))

(defun read-progn (stream char)
  (declare (ignore char))
  (cons 'progn
        (read-delimited-list #\} stream t)))

(defreadtable :curly-bracens
  (:merge :standard)
  (:macro-char #\} (lambda (&rest r) (declare (ignore r))) nil)
  (:macro-char #\{ 'read-progn nil))

(defvar return 'return)






(function do_thing () {
  return 1;
});

(function () {
  return 1;
});






(if (> 3 1) {
  (princ :hi)
  (terpri)
  (+ 2 3)
} else {
  (princ :bye)
  (terpri)
  (+ 4 5)
})
