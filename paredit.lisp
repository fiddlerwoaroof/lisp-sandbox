(defpackage :paredit
  (:use :cl)
  (:export ))
(in-package :paredit)

(defclass cursor ()
  ((%pos :initarg :pos :reader pos)))

(defun make-cursor (&rest path)
  (make-instance 'cursor
                 :pos (copy-seq path)))

(defun get-exp-at-cursor (cursor exp)
  (reduce (lambda (acc next-pos)
            (elt acc next-pos))
          (pos cursor)
          :initial-value exp))

(defun barf-backward (cursor exp)
  (let* ((parent-exp-cursor (apply 'make-cursor
                                   (butlast (pos cursor))))
         (parent-exp (get-exp-at-cursor parent-exp-cursor exp))
         (grandparent-exp-cursor (apply 'make-cursor
                                        (butlast (pos parent-exp-cursor)))))
    



    ))
