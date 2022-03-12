(defpackage :fwoar.lisp-sandbox.awk
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.awk)

(named-readtables:defreadtable :fwoar-awk
  (:macro-char #\@ 'read-col-designator t))

(defun read-col-designator (s _)
  (declare (ignore _))
  (let ((designator (read s t nil t)))
    `(resolve-column *client*
                     *record*
                     ,(etypecase designator
                        (fixnum designator)
                        (symbol (string-downcase designator))
                        (string designator)))))

(defmacro with-command-output ((s command &rest args &key (output nil output-p)
                                &allow-other-keys)
                               &body body)
  (declare (ignore output))
  (when output-p
    (error "can't override :output"))
  `(let ((,s (uiop:process-info-output
              (uiop:launch-program ,command :output :stream ,@args))))
     ,@body))

(defvar *eof-sentinel* '#:eof)
(defgeneric make-client (client source))
(defgeneric next-record (client source))
(defgeneric parse-record (client raw-record))
(defgeneric unpack-binders (client record))
(defgeneric field-count (client record))
(defgeneric resolve-column (client record column-designator))

(defclass lines ()
  ((%column-cache :initform (make-hash-table))))
(fw.lu:defclass+ stream-lines (lines)
  ())
(fw.lu:defclass+ string-lines (lines)
  ((%pos :initform 0 :accessor lines-pos)))

(defmethod resolve-column ((client lines) (record cons) (column-designator number))
  (let ((column-designator (1- column-designator)))
    (with-slots (%column-cache) client
      ;; manually tuned
      (if (> column-designator 60)
          (alexandria:ensure-gethash column-designator
                                     %column-cache
                                     (nth column-designator record))
          (nth column-designator record)))))
(defmethod parse-record ((client lines) (raw-record string))
  (serapeum:tokens raw-record))
(defmethod field-count ((client lines) (record list))
  (length record))
(defmethod unpack-binders ((client lines) (record list))
  record)


(defmethod make-client ((client (eql :lines)) (source stream))
  (stream-lines))
(defmethod make-client ((client (eql :lines)) (source string))
  (string-lines))

(defmethod next-record :before ((client lines) (source stream))
  (clrhash (slot-value client '%column-cache)))
(defmethod next-record ((client stream-lines) (source stream))
  (read-line source nil *eof-sentinel*))
(defmethod next-record ((client string-lines) (source string))
  (let ((next-newline (position #\newline source :start (lines-pos client))))
    (if (< (lines-pos client) (length source))
        (prog1 (subseq source (lines-pos client) next-newline)
          (setf (lines-pos client) (if next-newline
                                       (1+ next-newline)
                                       (length source))))
        *eof-sentinel*)))

(fw.lu:defclass+ ndjson ()
  ())

(defmethod make-client ((client (eql :ndjson)) (source stream))
  (ndjson))
(defmethod next-record ((client ndjson) (source stream))
  (let ((line (read-line source nil *eof-sentinel*)))
    (if (eql line *eof-sentinel*)
        line
        (let ((yason:*parse-json-arrays-as-vectors* t))
          (yason:parse line)))))
(defmethod resolve-column ((client ndjson) (record hash-table) column-designator)
  (gethash column-designator record))
(defmethod resolve-column ((client ndjson) (record vector) (column-designator number))
  (aref record (1- column-designator)))
(defmethod parse-record ((client ndjson) raw-record)
  raw-record)
(defmethod field-count ((client ndjson) record)
  0)
(defmethod field-count ((client ndjson) (record vector))
  (length record))
(defmethod unpack-binders ((client ndjson) record)
  nil)
(defmethod unpack-binders ((client ndjson) (record vector))
  (coerce record 'list))


(defvar *client*)
(defvar *record*)
(defvar *nr*)
(defvar *nf*)
(defmacro do-lines ((line s &optional (client :lines)) &body body)
  (multiple-value-bind (body decls)
      (alexandria:parse-body body)
    (alexandria:with-gensyms (client-instance)
      (alexandria:once-only (s)
        `(let* ((,client-instance (make-client ,client ,s))
                (*client* ,client-instance))
           (loop for ,line = (next-record *client* ,s)
                 until (eql ,line *eof-sentinel*)
                 do ((lambda (,line)
                       ,@decls
                       (let ((*client* ,client-instance))
                         ,@body))
                     ,line)))))))

(defmacro awk ((s &key (args nil args-p) (client :lines)) &body pattern-actions)
  (let* ((begin (when (eql (caar pattern-actions) :begin)
                  (car pattern-actions)))
         (end (when (eql (caar (last pattern-actions)) :end)
                (car (last pattern-actions))))
         (pattern-actions (if begin
                              (cdr pattern-actions)
                              pattern-actions))
         (pattern-actions (if end
                              (butlast pattern-actions)
                              pattern-actions))
         (binders (when args-p
                    (mapcar (lambda (n)
                              (intern (format nil "$~d" n)))
                            (alexandria:iota args :start 1)))))
    `(block nil
       ,@(cdr begin)
       (let ((*nr* 0))
         (do-lines ($0 ,s ,client)
           (declare (ignorable $0))
           (let* (($* (parse-record *client* $0))
                  (*record* $*)
                  (*nf* (field-count *client* $*)))
             (declare (ignorable $*))
             (destructuring-bind (&optional ,@binders &rest $@)
                 (unpack-binders *client* $*)
               (declare (ignorable $@ ,@binders))
               ,@(mapcar (lambda (it)
                           (if (= 1 (length it))
                               (alexandria:with-gensyms (v)
                                 `(let ((,v ,(car it)))
                                    (when ,v
                                      (princ $0)
                                      (terpri))))
                               (cons 'when it)))
                         pattern-actions)))
           (incf *nr*)))
       ,@(cdr end)
       (values))))

(defmacro defawk (name (s args) &body body)
  `(defun ,name (,s)
     (awk (,s :args ,args)
       ,@body)))

#+(or)
(
 (spinneret:with-html
   (:table
    (with-input-from-string (s (format nil "a b~%c d~% e f"))
      (awk (s :args 2)
        (:begin (:thead (:th "first") (:th "second")))
        (t (:tr (mapc (lambda (cell)
                        (:td $1 cell))
                      $*)))
        (:end (:tfoot (:td "end first") (:td "end second")))))))

 (spinneret:with-html
   (:table
    (awk ((format nil "a b~%c d~% e f~%g") :args 2)
      (t (:tr (mapc (lambda (cell)
                      (:td $1 cell))
                    $*)))
      (:end (:tfoot (:td "end first") (:td "end second"))))))

 (spinneret:with-html
   (:table
    (with-command-output (s "ps aux")
      (awk (s :args 9)
        (:begin (:thead (:th "first") (:th "second")))
        (t (:tr (:td $1) (:td $2) (:td $3 "%") (:td $4)
                (:td (serapeum:string-join $@ " "))))
        (:end (:tfoot (:td "end first") (:td "end second")))))))

 (serapeum:with-collector (c)
   (with-command-output (s "ps aux")
     (awk (s :args 10)
       ((> *nf* 30) (c *nf* (car $@))))))

 )
