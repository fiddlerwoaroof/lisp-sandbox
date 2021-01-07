(defpackage :fwoar.lisp-sandbox.eclector-test
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.eclector-test)

(defvar *verbose* nil)

(defclass my-reader ()
  ())
(defvar *eclector-client*
  (make-instance 'my-reader))

(fw.lu:defclass+ fw-package ()
  ((%name :initarg :name :reader fw-name)
   (%symbols :reader fw-symbols :initform (make-hash-table :test 'equal))))


(defvar *fw-packages*
  (alexandria:alist-hash-table (list (cons "KEYWORD" (fw-package "KEYWORD")))
                               :test 'equal))

(defmethod print-object ((o fw-package) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~s with ~d symbols"
            (fw-name o)
            (hash-table-count (fw-symbols o)))))

(defgeneric my-intern (symbol package)
  (:method ((symbol string) (package package))
    (intern symbol package))
  (:method ((symbol string) (package fw-package))
    (when *verbose*
      (:printv symbol package))
    (if (eql package (ensure-package "KEYWORD"))
        (intern symbol (find-package "KEYWORD"))
        (fw.lu:if-let* ((s-p (find-package (fw-name package)))
                        (s-s (and s-p (find-symbol symbol s-p)))
                        (_ (not (eql s-p (symbol-package s-s)))))
          (my-intern symbol
                     (ensure-package
                      (package-name
                       (symbol-package s-s))))
          (alexandria:ensure-gethash symbol
                                     (fw-symbols package)
                                     (make-symbol symbol))))))

(defgeneric my-find-symbol (symbol package)
  (:method ((symbol string) (package package))
    (find-symbol symbol package))
  (:method ((symbol string) (package fw-package))
    (gethash symbol (fw-symbols package))))

(defun my-find-package (package)
  (gethash package *fw-packages*))

(defun ensure-package (package)
  (etypecase package
    (fw-package package)
    (package (ensure-package (package-name package)))
    (symbol (ensure-package (symbol-name package)))
    (string (alexandria:ensure-gethash package *fw-packages*
                                       (fw-package package)))))

(defmethod eclector.reader:interpret-symbol
    ((client my-reader) input-stream package-indicator symbol-name internp)
  (let ((package (case package-indicator
                   (:current (ensure-package *package*))
                   (:keyword (ensure-package "KEYWORD"))
                   (t        (or (ensure-package package-indicator))))))
    (if *verbose*
        (:printv package-indicator package symbol-name (my-intern symbol-name package))
        (my-intern symbol-name package))))

(defparameter *my-readtable*
  (let ((readtable (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\+
     (lambda (stream char parameter)
       parameter
       char
       (:printv
        eclector.reader:*client*
        (let ((*verbose* t))
          (cons 'when-feature
                (eclector.reader:read stream t
                                      nil t))))))
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\-
     (lambda (stream char parameter)
       parameter
       char
       (:printv
        eclector.reader:*client*
        (let ((*verbose* t))
          (cons 'unless-feature
                (eclector.reader:read stream t
                                      nil t))))))
    readtable))

(defun my-read-from-string (string)
  (let ((eclector.reader:*client* *eclector-client*)
        (eclector.reader:*readtable* *my-readtable*))
    (eclector.reader:read-from-string string)))
