(in-package :cl-user)

(defmacro quickloads (&rest r)
  `(progn
     ,@(loop for x in r
             collect `(ql:quickload ,x))))

(quickloads
  :cl-actors
  :cells
  :contextl)

(defpackage :math-test
  (:use :cl :cl-actors :cells))

(in-package :math-test)

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
           (tax-rate :accessor tax-rate :initarg :tax-rate :initform (c-in 0.08))
           (tip      :accessor tip      :initarg :tip      :initform (c-in 0))
           (total    :accessor total    :initform (c? (+   (subtotal self)     
                                                           (* (tax-rate self)
                                                              (subtotal self))
                                                           (tip self))))))

(defobserver tip ((self tip-calc))
             (when old-value-boundp
               (format t "The tip is: ~a~%It changed by: ~a~%" new-value (- new-value
                                                                            old-value))))



(defmacro push-many (place &body items)
  `(progn ,@(loop for item in items
                  collect `(push ,item ,place))))

(defparameter s-r (make-instance 'service-rating))
(defparameter bill (make-instance 'bill))
(defparameter tc (make-instance 'tip-calc :cost (c? (cost bill)) :rate (c? (rate s-r))))
(defparameter meal-calc (make-instance 'meal-expense-calculator
                                       :subtotal (c? (cost bill))
                                       :tip      (c? (tip tc))))

(push-many (items bill)
  (make-instance 'item :kind :meatloaf :cost 12.99)
  (make-instance 'item :kind :salmon :cost 14.99)
  (make-instance 'item :kind :frenchfries :cost 3.99)
  (make-instance 'item :kind :tomatosoup :cost 3.99)
  (make-instance 'item :kind :burgundy :cost 8.99)
  (make-instance 'item :kind :icedtea :cost 3.99))
(setf (rating s-r) :excellent)

(format t "~{~a~20t~a~%~}------------------------------
Subtotal:~20t~a~%Tax:~20t~a~%Tip:~20t~a~%Total:~20t~a~%"
        (loop for item in (items bill)
              append (list (kind item) (cost item)))
        (subtotal meal-calc)
        (* 0.08 (subtotal meal-calc))
        (tip tc)
        (total meal-calc))
