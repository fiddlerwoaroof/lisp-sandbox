(defpackage :ssh-configurator
  (:use :clim :clim-lisp))
(in-package :ssh-configurator)

(defclass host ()
  ((%hosts :initarg :hosts :reader hosts)
   (%options :initarg :options :reader options)))
(defclass ssh-config ()
  ((%hosts :initarg :hosts :reader hosts)
   (%options :initarg :options :reader options)))

(defgeneric read-in (file object)
  (:method ((path string) object)
    (read-in (pathname path) object))
  (:method ((path pathname) object)
    (alexandria:with-input-from-file (f path)
      (read-in f object))))

(defmethod read-in (file (host-blocks))
  )

(defun prefix-count-if (pred seq)
  (length (serapeum:take-while pred seq)))

(defun read-indented-block (stream)
  (values (loop for line = (read-line stream nil)
             for next-char = (peek-char nil stream nil)
             for whitespace-count = (prefix-count-if #'serapeum:whitespacep line)
             when (< whitespace-count (length line)) collect
               (cons whitespace-count
                     (subseq line whitespace-count))
             while (and next-char
                        (serapeum:whitespacep next-char)))
          stream))

(defun tokenize-block (block)
  (mapcar (alexandria:compose 'serapeum:tokens
                              'cdr)
          block))

(defun read-all-indented-blocks (stream)
  (loop for block = (read-indented-block stream)
     while block
     collect block))

(define-application-frame ssh-configurator ()
  ((host-blocks :initarg :host-blocks :accessor host-blocks
                :initform (error "need host blocks...")))
  (:panes
   (hosts (make-pane 'list-pane :items (host-blocks *application-frame*)))
   (props (make-pane 'list-pane :items '(2 3 41 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))))
  (:layouts
   (default (vertically ()
              (scrolling ()
                hosts)
              (scrolling ()
                props))))
  )


(defun ssh-configurator (host-blocks)
  (fw.lu:prog1-bind (f (make-application-frame 'ssh-configurator
                                               :host-blocks host-blocks))
    (bt:make-thread
     (serapeum:op (run-frame-top-level f))
     :name "ssh-configurator")))

(defmacro print-and-return-when ((condition) form &rest others)
  `(let ((result ,form))
     (when ,condition
       (format *trace-output* "~&Result is: ~s~%~4t(~{~s~^ ~})~%" result (list ,@others)))
     result))

(defun get-all-subclasses (object)
  (print-and-return-when ((typep (car result) 'standard-class))
   (etypecase object
     (list (mapcar 'get-all-subclasses object))
     (null (princ :what?))
     (symbol (get-all-subclasses (find-class object)))
     ((or standard-class sb-mop:funcallable-standard-class)
      (list (class-name object)
            (get-all-subclasses (sb-mop:class-direct-subclasses object)))))
   ))

(defpackage :ssh-configurator/t
  (:use :cl :should-test))
(in-package :ssh-configurator/t)
(import 'ssh-configurator::read-indented-block)

(deftest read-indented-block ()
    ()
  (should be equal
          '((0 . "a b c") (4 . "d"))
          (with-input-from-string (s (format nil "a b c~%~4td"))
            (read-indented-block s)))
  (should be equal
          '((0 . "e f g") (4 . "h"))
          (with-input-from-string (s (format nil "a b c~%~4td~%e f g~%~4th"))
            (read-indented-block
             (nth-value 1 (read-indented-block s)))))
  (should be equal
          '((0 . "e f g") (4 . "h"))
          (with-input-from-string (s (format nil "a b c~%~4td~%e f g~%~4th~%  "))
            (read-indented-block
             (nth-value 1 (read-indented-block s)))))
  (should be equal
          nil
          (with-input-from-string (s (format nil ""))
            (read-indented-block
             (nth-value 1 (read-indented-block s)))))
  (should be equal
          nil
          (with-input-from-string (s (format nil "   "))
            (read-indented-block
             (nth-value 1 (read-indented-block s))))))
