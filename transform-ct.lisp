(ql:quickload '(:plump
		:lquery
		:serapeum
		:alexandria
		:flexi-streams
		:chipz
		:babel
		:net.didierverna.clon))

(in-package #:org.shirakumo.plump.parser)
(define-tag-dispatcher (script *tag-dispatchers* *html-tags*)
    (name)
    (string-equal name "script")
  (let* ((closing (consume))
         (attrs
          (if (char= closing #\ )
              (prog1 (read-attributes) (setf closing (consume)))
              (make-attribute-map))))
    (case closing
      (#\/ (advance) (make-element *root* "script" :attributes attrs))
      (#\>
       (let ((*root* (make-fulltext-element *root* "script" :attributes attrs)))
         (make-text-node *root*
                         (consume-until
                          (make-matcher
                           (or (is "</script>") (is "</SCRIPT>")))))
         (advance-n 9)
         *root*)))))

(in-package :cl-user)
(defpackage :ct-transform
  (:use :cl :lquery :serapeum :alexandria :net.didierverna.clon))
(in-package :ct-transform)

(defvar *version* "0.001")
(defsynopsis (:postfix "FILE")
  (group (:header "Generic options")
         (flag :short-name "v" :long-name "version"
               :description "Show the program version")
         (flag :short-name "h" :long-name "help"
               :description "Show this help")))


(defvar *txt* nil "The parsed HTML")
(defvar *fn* nil "The file to be pulled in")
(defvar *lookup-table* (make-hash-table :test 'equalp))

;; (uiop:directory-files "." (uiop:merge-pathnames* (make-pathname :type "bz2") uiop:*wild-file*))

(defun call-with-decompressed-text (fn cb &optional (encoding :iso-8859-1))
  (with-input-from-file (s fn :element-type '(unsigned-byte 8))
    (let* ((decompressing-stream (chipz:make-decompressing-stream 'chipz:bzip2 s))
	   (flexi-stream (flexi-streams:make-flexi-stream decompressing-stream :external-format encoding)))
      (unwind-protect (funcall cb flexi-stream)
	(close flexi-stream)
	(close decompressing-stream)))))

(defun lookup-ref (p q a &rest r)
  (gethash (format nil "~aq.~da.~d~{~a~}" (string-upcase p) q a r)
	   *lookup-table*))

(defun translate-book-ref (ref)
  (string-case ref
    ("" :st)
    ("CG" :scg)
    (t (make-keyword ref))))

(defun normalize-ref (ref)
  (destructuring-bind (book . ref) (split-sequence #\, ref)
    (if ref
	(setf ref (string-join ref ","))
	(setf ref book
	      book ""))
    (values (string-join (split-sequence #\space ref
					 :remove-empty-subseqs t))
	    (translate-book-ref (remove-if-not #'upper-case-p
					       (string-capitalize book))))))


(defun help ())
(defun show-version ()
  (format t "~&~a~%" *version*))

(declaim (ftype (function () nil) to-top))
(defun to-top ())
(defmacro mark-start (&body body)
  (with-gensyms (start)
    `(tagbody
	,start
	(flet ((to-top () (go ,start)))
	  ,@body))))

(defun transform-ct-main ()
  (make-context)
  (mark-start
    (restart-case
	(cond
	  ((getopt :long-name "help") (help))
	  ((getopt :long-name "version") (show-version))
	  (t (let ((file (car (remainder)))
		   (ofile (cadr (remainder)))
		   (*package* (find-package 'ct-transform)))
	       (lquery:initialize (call-with-decompressed-text file #'plump:parse))
	       (map 'list
		    (op (destructuring-bind (ref el) _
			  (setf (gethash (multiple-value-list (normalize-ref ref))
					 *lookup-table*)
				(plump:text el))))
		    ($ "p[title]" (combine (attr :title) (node))))

	       (let ((*print-case* :downcase))
		 (alexandria:with-output-to-file (*standard-output* ofile)
		   (loop for (ref book) being the hash-keys in *lookup-table* using (hash-value text)
		      do (print `(ref ,book ,ref
				      ,text)))))
	       ;; (alexandria:with-input-from-file (s *fn* :external-format :iso-8859-1)
	       ;;   (setf *txt* (plump:parse s)))

	       ;; (uiop:directory-files "." (uiop:merge-pathnames* (make-pathname :type "bz2") uiop:*wild-file*))
	       ;; (car *)
	       ;; (plump:parse *)
	       ;; (lquery:initialize *)
	       ;; ($ "p[title]" (combine (attr :title)
	       ;; 			    (text)))

	       )))
      (retry () (to-top))
      (abort ()))))

(defun make-executable ()
  (dump "transform-ct" transform-ct-main
        :compression 8
        :purify t))
