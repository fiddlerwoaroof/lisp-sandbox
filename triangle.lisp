(defpackage :fwoar.triangle
  (:use :cl )
  (:export ))
(in-package :fwoar.triangle)

(defun alternate-source ()
  (let ((alternator t))
    (lambda ()
      (prog1 alternator
        (setf alternator (not alternator))))))

(defun print-triangle (size)
  (let ((alt (alternate-source)))
    (dotimes (n size)
      (let ((count (1+ n)))
        (dotimes (i count)
          (format t "~:[0~;1~] " (funcall alt)))
        (terpri)))))

#|
TRIANGLE> (print-triangle 11)
1
0 1
0 1 0
1 0 1 0
1 0 1 0 1
0 1 0 1 0 1
0 1 0 1 0 1 0
1 0 1 0 1 0 1 0
1 0 1 0 1 0 1 0 1
0 1 0 1 0 1 0 1 0 1
0 1 0 1 0 1 0 1 0 1 0
|#
