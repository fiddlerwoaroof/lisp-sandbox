(defpackage :qtools-demo
  (:use :cl))

(in-package :qtools-demo)

(defun make-slider ()
  (prog1-bind (the-widget (make-instance 'qui:slider))
    ))

(defun main ()
  (cl+qt:with-main-window (w (make-instance 'qui:panel-container))
  (qui:add-widget (make-instance 'qui:panel :title "An empty panel :(") w)
  (qui:add-widget (make-instance 'qui:panel :title "A slider, whoa!"
                                 :center (make-slider)) w)
  (qui:add-widget (make-instance 'qui:listing :title "A slider, whoa!"
                                 :center (make-instance 'qui:slider)) w))
)
