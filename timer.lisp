(defpackage :timer
  (:use :clim-lisp :clim)
  (:export ))
(in-package :timer)

(define-application-frame timer ()
  ((time :initform 300))
  (:pointer-documentation t)
  (:panes (app :application :display-function 'display-timer :height 200 :width 600)
          (int :interactor :height 200 :width 600))
  (:layouts
   (default (vertically ()
              app int))))

(defun display-timer (frame pane)
  (with-text-style (pane '(:fixed :bold 18))
    (format pane "hello!")))
