(defpackage :css-norm
  (:use :cl))
(in-package :css-norm)

(defun read-block (stream)
  (when (char= (read-char stream) #\{)
    (loop
       with block = (make-string-output-stream)
       with count = 0
       for char = (read-char stream)
       until (and (char= char #\}) (= count 0))
       when (char= char #\{) do (incf count)
       when (char= char #\}) do (decf count)
       do (write-char char block)
       finally
	 (return (get-output-stream-string block)))))

(defun read-to-block (stream)
  (with-output-to-string (s)
    (loop
       until (char= #\{ (peek-char nil stream))
       do (write-char (read-char stream) s))))

(defun partition (char string &key from-end)
  (let ((pos (position char string :from-end from-end)))
    (if pos
	(list (subseq string 0 pos)
	      (subseq string (1+ pos)))
	(list nil
	      string))))

(defun parse-rule (block)
  (remove-if-not #'car
		 (mapcar (serapeum:op
			   (mapcar (serapeum:op
				     (and _1 (serapeum:trim-whitespace _1)))
				   (partition #\: _)))
			 (serapeum:split-sequence #\;
						  (serapeum:collapse-whitespace block)))))

(defun parse-selector (selector)
  (mapcar #'serapeum:trim-whitespace
	  (split-sequence:split-sequence #\, selector)))

(defun read-rule (stream)
  (cons (funcall (alexandria:compose #'parse-selector
				     #'serapeum:collapse-whitespace)
		 (read-to-block stream))
	(parse-rule (read-block stream))))

(defun parse-file (stream)
  (loop with result = (list)
     with done = nil
     until done
     do
       (handler-case (push (read-rule stream)
			   result)
	 (end-of-file (c) c (setf done t)))
     finally
       (return (nreverse result))))

(defun collapse-rule (rule)
  (let ((selector (car rule)))
    (mapcan (serapeum:op (mapcar (lambda (x) (list x _))
				 selector))
	    (cdr rule))))

(defun reconstitute (rules)
  (loop for (selector (property value)) in rules
       collect (format nil "~a { ~a: ~a; }" selector property value)))

(defun normalize-file (stream)
  (fw.lu:let-each (:be *)
    (parse-file stream)
    (mapcan #'collapse-rule *)
    (stable-sort * #'string< :key #'caadr)
    (reconstitute *)
    (serapeum:string-join * #\newline)))

(defun test-read-block ()
  (let ((strings (list "asdf cda qwer dsfa"
		       (format nil "asdf fdsaf ~% asdf qwerqw~%")
		       (format nil "{asdf fdsaf ~% asdf qwerqw~%}")
		       (format nil "asdf fdsaf {~% asdf qwerqw~%}"))))
    (loop
       for string in strings
       for n from 1
       do
	 (with-input-from-string (s (format nil "{~a}" string))
	   (format t "~&Test ~d: ~:[fail~;pass~]~%" n
		   (string= string (read-block s)))))))
