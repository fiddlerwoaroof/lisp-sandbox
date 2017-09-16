(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (ql:quickload '(:yason :alexandria :serapeum :fwoar.lisputils :osicat))
  (osicat-posix:setenv "CC" "/usr/bin/gcc")
  (ql:quickload :net.didierverna.clon))

(defpackage :json-to-table
  (:use :cl :alexandria :serapeum :fw.lu))
(in-package :json-to-table)

(defun hash-table-alist-rec (h-t)
  (map-tree 
   (op (typecase _1
	 (hash-table (hash-table-alist _1))
	 (t _1)))
   (hash-table-alist h-t)))

(defun get-all-keys (lis-ht &key (test 'equal))
  (reduce (op (union _ _ :test test))
	  lis-ht
	  :key #'hash-table-keys))

(defun tabulate (lis-ht stream)
  (let ((keys (get-all-keys lis-ht)))
    (format stream "~&~{~a~}~%~{~{~a~}~%~}"
	    (intersperse #\tab keys)
	    (mapcar (op (intersperse #\tab (pick keys _)))
		    lis-ht))))

(net.didierverna.clon:defsynopsis (:postfix "FILE")
  (text :contents "Read in a json array and print a table"))

(defun main ()
  (net.didierverna.clon:make-context)
  (let ((rem (net.didierverna.clon:remainder)))
    (if (null (cdr rem))
	(with-input-from-file (s (car rem))
	  (tabulate (yason:parse s) t))
	(error "only one arg allowed"))))


(defun build ()
  (net.didierverna.clon:dump "json-tabulator" main))
