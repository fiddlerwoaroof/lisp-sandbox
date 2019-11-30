#.(progn (ql:quickload :closer-mop) nil)

(defpackage :fwoar.simple-clone
  (:use :cl )
  (:export #:cloneable-class
           #:make-from))
(in-package :fwoar.simple-clone)

(defclass cloneable-class (c2mop:standard-class)
  ((%parent :initform (make-hash-table :test 'eq :weakness :key)
            :reader parent)))

(defmethod c2mop:validate-superclass ((class cloneable-class) (superclass c2mop:standard-class))
  t)

(defmethod c2mop:slot-boundp-using-class ((class cloneable-class) instance slotd)
  (or (call-next-method)
      (alexandria:when-let ((next (gethash instance (parent class))))
        (slot-boundp next
                     (c2mop:slot-definition-name slotd)))))

(defmethod c2mop:slot-value-using-class ((class cloneable-class) instance slotd)
  (block nil
    (handler-bind ((unbound-slot
                     (lambda (c)
                       (declare (ignore c))
                       (alexandria:when-let* ((next (gethash instance (parent class)))
                                              (value (slot-value next
                                                                 (c2mop:slot-definition-name slotd))))
                         (return (setf (c2mop:slot-value-using-class class instance slotd)
                                       value))))))
      (call-next-method))))

(defun make-from (a &rest r &key &allow-other-keys)
  (let* ((the-class (class-of a))
         (result (apply #'make-instance the-class r)))
    (prog1 result
      (setf (gethash result (parent the-class)) a))))

(defun relate (base derive)
  (let ((dict (parent (class-of derive))))
    (setf (gethash derive dict)
          base)))

