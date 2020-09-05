(defpackage :fwoar.binary-tree
  (:use :cl )
  (:export ))
(in-package :fwoar.binary-tree)

(fw.lu:defclass+ bt ()
  ((left :initarg :left :accessor left)
   (value :initarg :value :accessor value)
   (right :initarg :right :accessor right)))
(defmethod print-object ((o bt) s)
  (format s "[~s < ~s > ~s]"
          (left o)
          (value o)
          (right o)))

(defgeneric visit (client tree))
(defun visit-in-order (client tree)
  (let ((left (left tree))
        (right (right tree)))
    (when left
      (visit-in-order client (left tree)))
    (visit client tree)
    (when right
      (visit-in-order client (right tree))))
  tree)

(fw.lu:defclass+ counter ()
  ((value :initform 0 :accessor value)))
(defmethod visit ((client counter) tree)
  (setf (value tree)
        (value client))
  (incf (value client)))

(fw.lu:defclass+ stragglers ()
  ((elts :initarg :elts :accessor elts)))
(defmethod visit ((client stragglers) tree)
  (declare (optimize (debug 3)))
  (fw.lu:with-accessors* (elts) client
    (when (and elts (null (left tree)))
      (let ((n-l (pop elts))
            (n-r (pop elts)))
        (setf (left tree) (bt nil n-l nil))
        (when n-r
          (setf (right tree) (bt nil n-r nil)))))))

(defmethod visit ((client (eql :print)) tree)
  (fresh-line)
  (prin1 (value tree))
  (princ #\tab)
  (prin1 tree)
  (terpri))

(defun dense-tree (levels cur)
  (if (null cur)
      (let ((layer (loop repeat (expt 2 (1- levels)) collect (bt nil nil nil))))
        (dense-tree (1- levels)
                    layer))
      (if (> levels 0)
          (dense-tree (1- levels)
                      (loop for (l r) on cur by #'cddr
                            collect (bt l nil r)))
          (car cur))))

(defun complete-bt-of-size (n)
  (let* ((l2_n (log n 2))
         (height (ceiling l2_n))
         (max-node-count (1- (expt 2 height)))
         (full-bt-size (1- (expt 2 (1- height))))
         (leftovers (- n full-bt-size)))
    (values height max-node-count full-bt-size leftovers)))
