#!/usr/local/bin/sbcl --script
(declaim (optimize (speed 0) (safety 3) (debug 3)))
(load #p"~/quicklisp/setup.lisp")
(ql:quickload :qtools)
(ql:quickload :cells)
(ql:quickload :qtcore)
(ql:quickload :qtgui)
(ql:quickload :parse-number)
(ql:quickload :alexandria)
(ql:quickload :anaphora)

(cl:defpackage :tipcalc.models
  (:use :cl :cells)
  (:export
    service-rating fed-tip-calc tip-calc tax-calc item bill meal-expense-calculator
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
(defmodel fed-tip-calc (tip-calc)
          ((rate-feeder :cell t :accessor rate-feeder :initarg :rate-feeder)
           (rate :cell t :accessor rate :initarg :rate :initform (c? (rate (rate-feeder self))))))


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
  (:use :cl+qt :parse-number :anaphora :tipcalc.models))
(in-package :tipcalc)
(in-readtable :qtools)
;(defparameter *service-rating* (make-instance 'service-rating))
;(defparameter *tip-calc* (make-instance 'tip-calc :rate (rate *service-rating*)))

(define-widget edits (QWidget) ())
(define-widget service-quality (QWidget) ())
(define-widget main-window (QWidget) ())

(define-widget my-slider (QWidget) ())
(define-signal (my-slider val-changed) (float))
(define-subwidget (my-slider slider) (q+:make-qslider my-slider))
(define-subwidget (my-slider label) (q+:make-qlabel my-slider))
(define-subwidget (my-slider layout) (q+:make-qvboxlayout my-slider)
  (q+:add-widget slider)
  (q+:add-widget label))

(define-slot (my-slider slider) ((new-val integer))
  (declare (connected slider (value-changed integer)))
  (setf (q+:text label) (format nil "~a%" new-val))
  (signal! my-slider (val-changed float) (coerce (/ new-val 100) 'float)))

(defmethod set-value ((my-slider my-slider) (new-value float))
  (with-slots-bound (my-slider my-slider)
    (setf (q+:value slider) (round (* new-value 100)))))


(defmacro alambda (&body body)
  `(lambda (it) ,@body))

(defmacro with-no-signals (widget &body body)
  (alexandria:once-only (widget)
    `(progn
       (q+:block-signals ,widget t)
       (unwind-protect
         (progn
           ,@body)
         (q+:block-signals ,widget nil)))))

(defmacro connect-cell-to-widget ((widget sub-widget) (cell class) place widg-value-cb)
  "This connects a widget to an observable cell.  TODO: figure out the macro edge-cases here"
  `(cells:defobserver ,cell ((self ,class)) 
                      ; I want this to run:
                      ; EITHER When the old value is not bound
                      ; OR     When the old value is not equal to the new value
                      (format t "--> changed: ~a is not ~a~%" ,(symbol-name cell) cells:new-value)
                      (with-slots-bound (,widget ,widget)
                        (with-signals-blocked (,sub-widget) 
                          (setf ,place (funcall ,widg-value-cb cells:new-value))))))

(defmacro connect-widget-to-cell ((widget signal arg-type) place &key value-mod)
  (alexandria:with-gensyms (arg)
    `(define-slot (,widget ,signal) ((,arg ,arg-type))
       (declare (connected ,widget (,signal ,arg-type)))
       ,(when value-mod
          `(setf ,arg (funcall ,value-mod ,arg)))
       (setf ,place ,arg))))

(defmacro connecting ((widget sub-widget) (cell-slot-name cell-class) cell-instance &body body)
  `(macrolet ((cell->widget (place cb)
                `(connect-cell-to-widget (,',widget ,',sub-widget) (,',cell-slot-name ,',cell-class) ,place ,cb))
              (signal->cell (signal &key value-mod)
                `(connect-widget-to-cell (,',widget ,@signal) (,',cell-slot-name ,',cell-instance) :value-mod ,value-mod)))
     ,@body))

(defmacro define-connections (&rest connections)
  (list*
    'progn
    (loop for (widg-spec cell-spec cell-inst . body) in connections
          collect `(connecting ,widg-spec ,cell-spec ,cell-inst
                     ,@body))))  

(let* ((s-r (make-instance 'service-rating))
       (tc (make-instance 'tip-calc :rate (cells:c? (rate s-r)))))

  (defun stringify (obj) (format nil "~a" obj))
  (defun make-keyword (str)
    (alexandria:make-keyword (string-upcase str)))

  (define-signal (edits cost-changed)  (float))
  (define-signal (edits rate-changed)  (float))
  (define-signal (edits tip-changed)  (float))

  (define-subwidget (edits cost-widg) (q+:make-qlineedit edits)
    (setf (q+:placeholder-text cost-widg) (stringify (cost tc)))) 

  (connecting (edits cost-widg) (cost tip-calc) tc
    (signal->cell
      (cost-changed float)))

  (define-slot (edits cost-widg) ((new-text string))
    (declare (connected cost-widg (text-edited string)))
    (handler-case
      (let ((num (coerce (parse-number new-text) 'float)))
        (signal! edits (cost-changed float) num))
      ((or parse-error type-error) (c) (declare (ignore c)))))

  (define-subwidget (edits rate-widg) (q+:make-qslider edits)
    (setf (q+:minimum rate-widg) 6)
    (setf (q+:maximum rate-widg) 30)
    (setf (q+:value rate-widg) (round (* 100 (rate s-r)))))


  
  (connecting (edits rate-widg) (rate tip-calc) tc
    (cell->widget
      (q+:value rate-widg)
      (alambda
        (round (* 100 it))))
    (signal->cell
      (rate-changed float))) 


  (define-slot (edits rate-widg) ((new-value integer))
    (declare (connected rate-widg (value-changed integer)))
    (handler-case
      (signal! edits (rate-changed float) (coerce (/ new-value 100) 'float))
      ((or parse-error type-error) (c) (declare (ignore c)))))


  (define-subwidget (edits tip-widg) (q+:make-qlineedit edits)
    (setf (q+:placeholder-text tip-widg) (stringify (tip tc))))

  (connecting (edits tip-widg) (tip tip-calc) tc
    (cell->widget
      (q+:text tip-widg)
      (alambda
        (stringify it))))
  

  (define-subwidget (edits edit-layout) (q+:make-qhboxlayout edits)
    (q+:add-widget edit-layout cost-widg)
    (q+:add-widget edit-layout rate-widg)
    (q+:add-widget edit-layout tip-widg))

  (define-signal (service-quality quality-chosen) (string))

  (define-subwidget (service-quality choose-poor) (q+:make-qradiobutton service-quality)
    (setf (q+:text choose-poor) "Poor"))
  (define-slot (service-quality choose-poor) ()
    (declare (connected choose-poor (toggled bool)))
    (signal! service-quality (quality-chosen string) "poor"))

  (define-subwidget (service-quality choose-normal) (q+:make-qradiobutton service-quality)
    (setf (q+:text choose-normal) "Normal"))
  (define-slot (service-quality choose-normal) ()
    (declare (connected choose-normal (toggled bool)))
    (signal! service-quality (quality-chosen string) "normal"))


  (define-subwidget (service-quality choose-excellent) (q+:make-qradiobutton service-quality)
    (setf (q+:text choose-excellent) "Excellent"))
  (define-slot (service-quality choose-excellent) ()
    (declare (connected choose-excellent (toggled bool)))
    (signal! service-quality (quality-chosen string) "excellent"))

  (connecting (service-quality nil) (rating service-rating) s-r
    (signal->cell
      (quality-chosen string)
      :value-mod (alambda (make-keyword it))))

  (define-subwidget (service-quality quality-layout) (q+:make-qhboxlayout service-quality)
    (q+:add-widget quality-layout choose-poor)
    (q+:add-widget quality-layout choose-normal)
    (q+:add-widget quality-layout choose-excellent))

  (define-subwidget (main-window edits) (make-instance 'edits))
  (define-subwidget (main-window service-quality) (make-instance 'service-quality))
  (define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
    (q+:add-widget layout service-quality) 
    (q+:add-widget layout edits)))


(defun main ()
  (with-main-window (window (make-instance 'main-window))))

(ql:quickload  :swank)
(swank:create-server :port 4006)
(handler-case (main)
  (sb-sys:interactive-interrupt (c) (declare (ignore c)) (format t "~cExiting on interrupt...~%" #\return)))

