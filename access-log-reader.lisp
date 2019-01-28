(defpackage :access-log-reader
  (:shadowing-import-from :multi-fun :rest)
  (:shadowing-import-from :data-lens :pick :defun-ct)
  (:use :cl :multi-fun :data-lens :alexandria :fw.lu)
  (:export ))
(in-package :access-log-reader)

(defun make-timestamp (record)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (destructuring-bind (day month year time) record
      (local-time:with-decoded-timestamp (:hour hour :minute minute :sec second :nsec nsec) time
        (local-time:encode-timestamp nsec second minute hour day month year)))))

(defun extract-query-params (s)
  (serapeum:mapply 'cons
                   (map 'list 
                        (serapeum:op (coerce (fwoar.string-utils:split #\= _ :count 2) 'list))
                        (fwoar.string-utils:split #\& s))))
(defparameter +access-log-parser+
  (list (delimited-field #\:)
        (delimited-field #\space) 
        (as (delimited-field #\space) (lambda (s) (subseq s 1 (1- (length s)))))
        (as (delimited-field #\space) (lambda (s) (unless (equal s "-") (parse-integer s))))
        (as (delimited-field #\space) (lambda (s) (unless (equal s "-") (parse-integer s))))
        (as (delimited-field #\space) #'parse-integer)
        (as (delimited-field #\space) (lambda (s) (unless (equal s "-") (parse-integer s))))
        (as (subformat (v (ignore-char #\[)
                          (as (delimited-field #\/) #'parse-integer)
                          (as (delimited-field #\/) (serapeum:op (position _ local-time:+short-month-names+ :test 'equal)))
                          (as (delimited-field #\:) #'parse-integer)
                          (as (delimited-field #\]) (lambda (x) (local-time:parse-timestring (remove #\space x) :allow-missing-date-part t)))))
            'make-timestamp)
        (whitespace)
        (as (delimited-field #\space) (lambda (s) (subseq s 1)))
        (splat-result (as (delimited-field #\space)
                          (alexandria:compose
                           (data-lens:juxt 'quri:uri-path
                                           (alexandria:compose 'extract-query-params 'quri:uri-query))
                           'quri:uri)))
        (delimited-field #\")
        (delimited-field #\space)
        (delimited-field #\space)
        (delimited-field #\space)
        (ignore-char #\")
        (as (delimited-field #\") 'quri:uri)
        (whitespace)
        (rest)))

(defun parse-log (f)
  (parse-file +access-log-parser+ f))
