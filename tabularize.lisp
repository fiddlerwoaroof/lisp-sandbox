(defpackage :tabularize
  (:use :cl :alexandria :fw.lu :fwoar.string-utils :net.didierverna.clon)
  (:export ))
(in-package :tabularize)

(defun read-n-lines (lines stream)
  (loop for n below lines
     for line = (read-line stream nil)
     while line
     collect line))

(defun maximize-cell (part-lens)
  (reduce (lambda (accum next)
            (map 'vector #'max accum next))
          part-lens
          :initial-value (fill (make-array (length (car part-lens)))
                               0)))

(defun left-pad (str len)
  (concatenate 'string
               (make-string (- len (length str)) :initial-element #\space)
               str))

(defun normalize-chunk (lines separator)
  (let* ((parts (mapcar (lambda (line) (split separator line))
                        lines))
         (part-lens (mapcar (lambda (line)
                              (map 'vector #'length line))
                            parts))
         (pad-to (maximize-cell part-lens)))
    (map 'list (lambda (line)
                 (map 'list (lambda (part len) (left-pad part (1+ len)))
                      line
                      pad-to))
         parts)))

(defun tabularize (stream &key (separator #\tab) (chunk-length 25))
  (loop for lines = (read-n-lines chunk-length stream)
     for normalized = (normalize-chunk lines separator)
     while lines
     do (format t "~&~{~{~a~}~%~}" normalized))
  (values))

(defsynopsis ()
  (text :contents "A program for tabularizing data")
  (stropt :short-name "s" :long-name "separator"
        :description "Separator between fields: must be a single character"
        :default-value (format nil "~c" #\tab))
  (lispobj :short-name "c" :long-name "chunk-length"
        :description "The number of lines to format as a unit"
        :typespec 'positive-integer
        :default-value 25)
  (flag :long-name "help"
        :short-name "h"
        :description "Print help"))

(defun main ()
  (make-context)
  (cond ((getopt :long-name "help") (help))
        (t (tabularize *standard-input*
                       :separator (getopt :long-name "separator")
                       :chunk-length (getopt :long-name "chunk-length")))))

(defun make-executable ()
  (dump "tabularize" main
        :compression 8))
