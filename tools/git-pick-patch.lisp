(defpackage :git-pick-patch
  (:use :cl :alexandria :serapeum)
  (:export ))
(in-package :git-pick-patch)

(defun read-header (inp)
  (string-join (loop for line = (read-line inp nil)
                  while line
                  collect line
                  until (eql #\@ (peek-char nil inp nil)))
               #\newline))

(defun read-hunk (inp)
  (when (eql #\@
             (peek-char nil inp nil))
    (string-join (loop for line = (read-line inp nil)
                    while line
                    collect line
                    until (member (peek-char nil inp nil) '(#\@ #\d)))
                 #\newline)))

(defun read-hunks (inp)
  (loop for hunk = (read-hunk inp)
     while hunk
     collect hunk))

(defun get-file-patch (inp)
  (list (read-header inp)
        (read-hunks inp)))

(defun get-all-patches (inp)
  (loop for patch = (get-file-patch inp)
     for (header data) = patch
     while (and (string/= header "")
                (not (null data)))
     collect patch))

(defun filter-hunks (hunks predicate)
  (remove-if-not predicate hunks))

(defun filter-file-hunks (file-data predicate)
  (let ((results (filter-hunks (cadr file-data)
                               predicate)))
    (when results
      (list (car file-data)
            results))))

(defun filter-patch (patch-data predicate)
  (remove-if #'null (mapcar (lambda (x)
                              (filter-file-hunks x predicate))
                            patch-data)))

(defun combine-hunks (hunks)
  (string-join hunks #\newline))

(defun rebuild-file-patch (file-data)
  (destructuring-bind (header hunks) file-data
    (format nil "~a~%~a" header (combine-hunks hunks))))

(defun rebuild-patch (patch-data)
  (string-join (mapcar #'rebuild-file-patch patch-data)
               #\newline))

(defun main ()
  (if (null (cadr sb-ext:*posix-argv*))
      (format t "~&Must provide a pattern!")
      (let* ((pattern (cadr sb-ext:*posix-argv*)))
        (loop for patch = (get-file-patch *standard-input*)
           for filtered = (when patch (filter-file-hunks patch
                                                         (op (cl-ppcre:scan pattern _))))
           until (equal patch '("" nil))
           when filtered do
             (format t "~&~a~&" (rebuild-file-patch filtered))))))
