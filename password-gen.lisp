;; from Rosetta code, with modifications https://rosettacode.org/wiki/Password_generator#Common_Lisp
(defpackage :fwoar.password-gen
  (:use :cl )
  (:export ))
(in-package :fwoar.password-gen)

(defparameter *lowercase* "abcdefghijklmnopqrstuvwxyz")

(defparameter *uppercase* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defparameter *numbers* "1234567890")

(defparameter *special-characters*
  '(#\! #\\ #\# #\% #\& #\* #\+ #\, #\- #\. #\: #\< #\= #\>
    #\? #\^ #\_ #\| #\~))

(defparameter *similar-characters*
  '(#\I #\l #\1 #\| #\O #\0 #\5 #\S #\2 #\Z))

(defparameter *excluded-characters*
  '(#\@ #\! #\[ #\] #\{ #\} #\/ #\\ #\' #\" #\& #\< #\> #\`))

(defun make-readable (flag s)
  (if flag
      (remove-if (lambda (x)
                   (member x *similar-characters*))
                 s)
      s))

(defun shuffle-seq (input-seq)
  (loop with l = (length input-seq)
        for i below l
        do (rotatef (elt input-seq i)
                    (elt input-seq
                         (random l))))
  input-seq)

(defun sample (seq)
  (elt seq
       (random (length seq))))

(defun generate-password (len human-readable exclude-excluded)
  (let* ((upper (make-readable human-readable *uppercase*))
         (lower (make-readable human-readable *lowercase*))
         (number (make-readable human-readable *numbers*))
         (special-initial (make-readable human-readable *special-characters*))
         (special (if exclude-excluded
                      (set-difference special-initial *excluded-characters*)
                      special-initial))
         (character-groups (list upper lower number special))
         (initial-password (reduce (lambda (acc x)
                                     (cons (sample x) acc))
                                   character-groups
                                   :initial-value NIL)))

    (coerce (shuffle-seq
             (reduce (lambda (acc x)
                       (declare (ignore x))
                       (let ((group (nth (random (length character-groups))
                                         character-groups)))
                         (cons (sample group)
                               acc)))
                     (make-list (- len 4))
                     :initial-value initial-password))
            'string)))

(defun main (len count &optional human-readable exclude-excluded)
  (if (< len 4)
      (print "Length must be at least 4~%")
      (loop for x from 1 to count do
        (princ (generate-password len human-readable exclude-excluded))
        (terpri))))

(defun parse-bool (str)
  (case (elt (string-downcase str) 0)
    ((#\t #\y) t)
    ((#\f #\n) nil)))

