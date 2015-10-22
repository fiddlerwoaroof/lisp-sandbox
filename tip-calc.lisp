(ql:quickload :qtools)
(ql:quickload :cells)
(ql:quickload :qtcore)
(ql:quickload :qtgui)

(cl:defpackage :tipcalc.models
  (:use :cl :cells)
  (:export
    service-rating tip-calc tax-calc item bill meal-expense-calculator
    rating rate cost tip tax kind item subtotal subtotal total))

(in-package :tipcalc.models)

(defmodel service-rating ()
          ((rating :cell t :accessor rating :initarg :rating :initform (c-in :normal))
           (rate   :cell t :accessor rate   :initform (c? (case (rating self)
                                                            (:normal 0.18)
                                                            (:excellent 0.25)
                                                            (:poor 0.15))))))

(defmodel tip-calc ()
          ((cost :cell t :accessor cost :initarg :cost :initform (c-in 0))
           (rate :cell t :accessor rate :initarg :rate :initform (c-in 0.18))
           (tip  :cell t :accessor tip  :initform (c? (* (cost self)
                                                         (rate self))))))

(defmodel tax-calc ()
          ((cost :cell t :accessor cost :initarg :cost :initform (c-in 0))
           (rate :cell t :accessor rate :initarg :rate :initform (c-in 0.18))
           (tax  :cell t :accessor tax  :initform (c? (* (cost self)
                                                         (rate self))))))

(defmodel item ()
          ((kind :cell t :accessor kind :initarg :kind :initform (c-in :food))
           (cost :cell t :accessor cost :initarg :cost :initform (c-in 0))))

(defmodel bill ()
          ((items :accessor items :initarg :items :initform (c-in nil))
           (cost  :accessor cost  :initform (c? (apply #'+
                                                       (loop for item in (items self)
                                                             collect (cost item)))))))

(defmodel meal-expense-calculator ()
          ((subtotal :accessor subtotal :initarg :subtotal :initform (c-in 0))
           (tax      :accessor tax      :initarg :tax      :initform (c-in 0.08))
           (tip      :accessor tip      :initarg :tip      :initform (c-in 0))
           (total    :accessor total    :initform (c? (+   (subtotal self)     
                                                           (* (tax-rate self)
                                                              (subtotal self))
                                                           (tip self))))))

(defpackage tipcalc
  (:use :cl+qt :tipcalc.models))
(in-package :tipcalc)
(in-readtable :qtools)

(define-widget main-window (QWidget) ())

(define-subwidget (main-window cost-widg) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text cost-widg) "Meal Cost?"))

(define-subwidget (main-window rate-widg) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text rate-widg) "Tip Rate?"))

(define-subwidget (main-window tip-widg) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text tip-widg) "Tip . . ."))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout cost-widg)
  (q+:add-widget layout rate-widg)
  (q+:add-widget layout tip-widg))

(defparameter *tip-calc* (make-instance 'tip-calc))
(cells:defobserver tip  ((self tip-calc))
                   ; I want this to run:
                   ; EITHER When the old value is not bound
                   ; OR     When the old value is not equal to the new value
                   (when (or (not old-value-boundp) (/= new-value old-value))
                     (setf (q+:text)
                         (format nil "~a" new-value))))


(defun main ()
  (with-main-window (window (make-instance 'main-window))))

