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


(fw.lu:defclass+ replace-char ()
  ((%point :initarg :point :accessor point)
   (%value :initarg :value :reader value)))
(defmethod transform-for-op ((op replace-char))
  (let ((point (point op))
        (insert-length (length (value op))))
    (lambda (new-point)
      (if (<= new-point point)
          new-point
          (+ new-point -1 insert-length)))))

(fw.lu:defclass+ insert ()
  ((%point :initarg :point :accessor point)
   (%value :initarg :value :reader value)))
(defmethod transform-for-op ((op insert))
  (let ((point (point op))
        (insert-length (length (value op))))
    (lambda (new-point)
      (if (< new-point point)
          new-point
          (+ new-point insert-length)))))

(fw.lu:defclass+ editable-string ()
  ((%string :initarg :string :accessor string-to-edit)
   (%transform :accessor transform :initform 'identity)))

(defmethod apply-edits ((base string) (operations sequence))
  (string-to-edit
   (reduce 'edit
           operations
           :initial-value (editable-string base))))

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
  "ac"
  '(insert "b" 1)
  "abc"
  '(insert "d" 2)
  "abdc")
