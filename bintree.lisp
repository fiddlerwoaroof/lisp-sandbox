(defpackage :bintree
  (:use :cl )
  (:export ))
(in-package :bintree)

(defclass bintree ()
  ((value :initarg :value :accessor node-value)
   (%left :initarg :left :accessor tree-left :initform nil)
   (%right :initarg :right :accessor tree-right :initform nil)))

(define-condition constraint-violated (serious-condition)
  ())

(defmethod (setf tree-left) :before ((new-value bintree) (object bintree))
  (unless (<= (node-value new-value)
              (node-value object))
    (error 'constraint-violated)))

(defmethod (setf tree-left) ((new-value integer) (object bintree))
  (setf (tree-left object) (make-instance 'bintree :value new-value)))

(defmethod (setf tree-right) :before ((new-value bintree) (object bintree))
  (unless (> (node-value new-value)
             (node-value object))
    (error 'constraint-violated)))

(defmethod (setf tree-right) ((new-value integer) (object bintree))
  (setf (tree-right object) (make-instance 'bintree :value new-value)))

(defun insert (tree value)
  (if (<= value (node-value tree))
      (if (null (tree-left tree))
          (setf (tree-left tree) value)
          (insert (tree-left tree) value))
      (if (null (tree-right tree))
          (setf (tree-right tree) value)
          (insert (tree-right tree) value))))

(defun rotate (tree direction)
  (ecase direction
    (:right (let* ((right-child (tree-right tree))
                   (left-of-right (tree-left right-child)))
              (setf (tree-left right-child) tree
                    (tree-right tree) left-of-right)
              right-child))
    (:left (let* ((left-child (tree-left tree))
                  (right-of-left (tree-right left-child)))
             (setf (tree-right left-child) tree
                   (tree-left tree) right-of-left)
             left-child))))

(defun list->tree (list)
  (declare (optimize (debug 3)))
  (destructuring-bind (root . rest) list
    (let ((result (make-instance 'bintree :value root)))
      (mapcar (lambda (v) (insert result v))
              rest)
      result)))

(defmethod print-object ((object bintree) stream)
  (labels ((print-node (node stream)
             (if node
                 (format stream "~a (~a) (~a)"
                         (node-value node)
                         (print-node (tree-left node) nil)
                         (print-node (tree-right node) nil))
                 (format stream "~a" node))))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~a)" (print-node object nil)))))
