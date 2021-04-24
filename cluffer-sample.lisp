(defpackage :fwoar.lisp-sandbox.cluffer-sample
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.cluffer-sample)

#| from:
;;   https://github.com/robert-strandh/Second-Climacs/ |#
(defun make-empty-standard-buffer-and-cursor ()
  (let* ((line (make-instance 'cluffer-standard-line:closed-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer
                                :initial-line line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (values buffer cursor)))

(defun fill-buffer-from-stream (cursor stream)
  (loop for char = (read-char stream nil nil)
        until (null char)
        do (cluffer:insert-item cursor char)
        when (eql char #\newline)
          do (cluffer:split-line cursor)))
#| End stolen code |#

(defmacro with-file-buffer ((buffer cursor file) &body body)
  (alexandria:with-gensyms (stream)
    `(multiple-value-bind (,buffer ,cursor) (make-empty-standard-buffer-and-cursor)
       (with-open-file (,stream ,file)
         (fill-buffer-from-stream ,cursor ,stream))
       ,@body)))
(defmacro with-stream-buffer ((buffer cursor stream) &body body)
  (alexandria:with-gensyms (s)
    `(multiple-value-bind (,buffer ,cursor) (make-empty-standard-buffer-and-cursor)
       (with-open-stream (,s ,stream)
         (fill-buffer-from-stream ,cursor ,stream))
       ,@body)))

(defun buffer-lines (buffer)
  (loop with cursor = (make-instance 'cluffer-standard-line:right-sticky-cursor)
        for x below (cluffer:line-count buffer)
        for line = (cluffer:find-line buffer x)
        do (cluffer:attach-cursor cursor line)
        collect (coerce (cluffer:items cursor) 'string)
        do (cluffer:detach-cursor cursor)))

(defun buffer-to-string (buffer)
  (serapeum:string-join
   (loop with cursor = (make-instance 'cluffer-standard-line:right-sticky-cursor)
         for x below (cluffer:line-count buffer)
         for line = (cluffer:find-line buffer x)
         do (cluffer:attach-cursor cursor line)
         collect (coerce (cluffer:items cursor) 'string)
         do (cluffer:detach-cursor cursor))
   ""))

(defun get-all-cursors (tree buffer)
  (labels ((int (tree sc ec)
             (destructuring-bind (_ __ pos children)
                 (fwoar.lisp-sandbox.tree-sitter-parser::parse-thing tree)
               (declare (ignore _ __))
               (destructuring-bind
                   ((start-c start-l) (end-c end-l)) pos
                 (let ((cursor1 (make-instance
                                 'cluffer-standard-line:right-sticky-cursor))
                       (cursor2 (make-instance
                                 'cluffer-standard-line:right-sticky-cursor)))
                   (cluffer:attach-cursor cursor1
                                          (cluffer:find-line buffer start-l)
                                          start-c)
                   (cluffer:attach-cursor cursor2
                                          (cluffer:find-line buffer end-l)
                                          end-c)
                   (funcall sc cursor1)
                   (funcall ec cursor2)
                   (when children
                     (mapcar (lambda (it)
                               (int it sc ec))
                             children)))))))
    (serapeum:with-collectors (sc ec)
      (int tree #'sc #'ec))))

(defun insert-guillemets (tree buffer)
  (multiple-value-bind (starts ends)
      (get-all-cursors tree buffer)
    (mapcar (lambda (start end)
              (cluffer:insert-item start #\«)
              (cluffer:insert-item end #\»))
            starts
            ends)))

(defun main-s (file-contents)
  (with-input-from-string (s file-contents)
    (with-stream-buffer (buffer cursor s)
      (insert-guillemets (cl-tree-sitter:parse-string :tsx file-contents)
                         buffer)
      (buffer-to-string buffer))))

(defun main (file)
  (let ((file-contents (alexandria:read-file-into-string file)))
    (with-file-buffer (buffer cursor file)
      (insert-guillemets (cl-tree-sitter:parse-string :tsx file-contents)
                         buffer)
      (buffer-to-string buffer))))

#|CLUFFER-SAMPLE> (princ (main "~/foo.ts"))
;;««interface «Foo» «{
;;  ««it»«: «string»»»;
;;  ««ot»«: ««Record»«<«string», «never»>»»»»;
;;}»»
;;
;;«const ««Bar»«: «Foo»» = «{
;;  ««it»: «'asdfasf'»»,
;;  ««ot»: «{}»»,
;;}»»»
;;» |#
