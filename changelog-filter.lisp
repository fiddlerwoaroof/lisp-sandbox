(defpackage :changelog-filter
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export ))
(in-package :changelog-filter)

(defparameter *changelog-path* "/Users/elangley/WebKit/Source/WebCore/ChangeLog")

(defun get-block (stream)
  (let ((header (read-line stream)))
    (loop for next = (peek-char nil stream nil)
       while (and next (whitespacep next))
       collect (read-line stream) into lines
       finally
         (return (trim-whitespace
                  (string-join (cons header lines)
                               #\newline))))))

(defun main ()
  (let ((*changelog-path* (or (caddr sb-ext:*posix-argv*)
                              *changelog-path*))
        (search-string (cadr sb-ext:*posix-argv*)))
    (handler-case (with-input-from-file (s *changelog-path*)
                    (loop for next-block = (get-block s)
                       when (search search-string next-block :test #'char-equal) do
                         (format t "~&~a~%" next-block)))
      (end-of-file (c) c))))

;;; sbcl --disable-debugger --no-userinit --load $HOME/quicklisp/setup.lisp --eval '(ql:quickload (list :alexandria :serapeum :fwoar.lisputils))' --load /Users/elangley/git_repos/lisp-sandbox/changelog-filter.lisp --eval "(save-lisp-and-die "'"'"changelog-filter"'"'" :executable t :toplevel #'changelog-filter::main :compression t)"
