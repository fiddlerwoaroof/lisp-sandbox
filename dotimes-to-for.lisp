(defpackage :dotimes-printer
  (:use :cl))
(in-package :dotimes-printer)

(dotimes (var count &optional result)
  &body body)

(defmacro destructure-dotimes ((var count &optional result) &body body)
  `(translate
    (produce-initializer ',var 0)
    (produce-check ',var '< ',count)
    (produce-increment ',)))

(defun translate-dotimes (form)
  ())
