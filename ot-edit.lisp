(defpackage :fwoar.lisp-sandbox.ot-edit
  (:use :cl )
  (:export
   #:edit
   #:insert
   #:editable-string
   #:apply-edits))
(in-package :fwoar.lisp-sandbox.ot-edit)

(defgeneric transform-for-op (op))
(defgeneric edit (base operation))
(defgeneric apply-edits (base operations))

(fw.lu:defclass+ editable-string ()
  ((%string :initarg :string :accessor string-to-edit)
   (%transform :accessor transform :initform 'identity)))

(defclass op ()
  ((%epoch :reader epoch :initform (let ((c (load-time-value (vector 0)))) (incf (elt c 0))))))

(defmethod apply-edits ((base string) (operations sequence))
  (string-to-edit
   (reduce 'edit
           (sort operations '< :key 'epoch)
           :initial-value (editable-string base))))

(fw.lu:defclass+ insert (op)
  ((%point :initarg :point :accessor point)
   (%value :initarg :value :reader value)))
(defmethod print-object ((o insert) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~d/~d" (point o) (epoch o))))
(defmethod transform-for-op ((op insert))
  (let ((point (point op))
        (insert-length (length (value op))))
    (lambda (new-point)
      (if (< new-point point)
          new-point
          (+ new-point insert-length)))))
(defun do-insert (base new point)
  (let ((begin (subseq base 0 point))
        (end (subseq base point)))
    (concatenate 'string begin new end)))
(defmethod edit ((base editable-string) (operation insert))
  (let ((current-transform (transform base))
        (op-point (point operation))
        (string-to-edit (string-to-edit base)))
    (setf (point operation) (funcall current-transform op-point)
          (string-to-edit base) (do-insert string-to-edit (value operation) (point operation))
          (transform base) (alexandria:compose (transform-for-op operation) current-transform))
    base))

(fw.lu:defclass+ replace-char (op)
  ((%point :initarg :point :accessor point)
   (%value :initarg :value :reader value)))
(defmethod print-object ((o replace-char) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~d/~d" (point o) (epoch o))))
(defmethod transform-for-op ((op replace-char))
  (let ((point (point op))
        (insert-length (length (value op))))
    (lambda (new-point)
      (if (<= new-point point)
          new-point
          (+ new-point -1 insert-length)))))
(defun do-replace-char (base new point)
  (let ((begin (subseq base 0 point))
        (end (subseq base (1+ point))))
    (concatenate 'string begin new end)))
(defmethod edit ((base editable-string) (operation replace-char))
  (let ((current-transform (transform base))
        (op-point (point operation))
        (string-to-edit (string-to-edit base)))
    (setf (point operation) (funcall current-transform op-point)
          (string-to-edit base) (do-replace-char string-to-edit (value operation) (point operation))
          (transform base) (alexandria:compose (transform-for-op operation) current-transform))
    base))


(defun sample ()
  (assert (equal (apply-edits "ac" (list (insert 0 "z")
                                         (insert 1 "b")
                                         (insert 2 "d")))
                 "zabcd")))
