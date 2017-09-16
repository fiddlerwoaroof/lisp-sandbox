(defpackage :fwoar.state
  (:use :cl))

(defun state-machine ()
  (declare (optimize (debug 3)))
  (labels
      ((backslash (char))
       (open-parens (char)
	 (case char
	   (#\) (list (list :paren) 'default t t))
	   (t (list (list :char char) 'open-parens nil t))))
       (quote% (char))
       (default (char)
	 (case char
	   (#\( (list (list :paren) 'open-parens nil t))
	   (t (list (list :char char) 'default nil t)))))
    (loop with state = 'default
       with result = (make-array 10 :adjustable t :fill-pointer 0)
       for next-char = (peek-char nil *standard-input* nil :eof)
       for (out next-state finished advance) = (funcall state next-char)
       until (or finished (eq next-char :eof) (null next-state))
       when advance do
	 (read-char *standard-input* nil :eof)
       when out do
	 (vector-push-extend out result)
       do
	 (setf state next-state)
       finally
	 (return result))))
