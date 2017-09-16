(defpackage :zfs-cleaner.utils
  (:use :cl)
  (:export #:regex-match #:include #:exclude #:pick
           #:snapshot-to-vector #:vector-to-lt #:key-transform
           #:combine #:derive #:cumsum #:over #:on #:shortcut
           #:defun-ct))
(in-package :zfs-cleaner.utils)

(defmacro shortcut (name function &body bound-args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (fdefinition ',name)
           (,function ,@bound-args))))

(defmacro defun-ct (name (&rest args) &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (defun ,name ,args
       ,@body)))

(defun-ct regex-match (regex)
  (lambda (data)
    (cl-ppcre:scan-to-strings regex data)))

(defun-ct include (pred)
  (lambda (seq)
    (remove-if-not pred seq)))

(defun-ct exclude (pred)
  (lambda (seq)
    (remove-if pred seq)))

(defun-ct pick (selector)
  (lambda (seq)
    (map 'list selector seq)))

(defun-ct key-transform (fun key-get key-set)
  (lambda (it)
    (let ((key-val (funcall key-get it)))
      (funcall key-set
               (funcall fun key-val)))))

(defun-ct combine (fun1 fun2)
  (lambda (item)
    (list (funcall fun1 item)
          (funcall fun2 item))))

(defun-ct derive (diff-fun &key (key #'identity))
  (lambda (list)
    (mapcar (lambda (next cur)
              (cons (funcall diff-fun (funcall key next) (funcall key  cur))
                    next))
            (cdr list)
            list)))

(defun-ct cumsum (&key (add-fun #'+) (key #'identity) (combine (lambda (x y) y x)) (zero 0))
  (lambda (seq)
    (nreverse
     (reduce (lambda (accum next)
               (let ((key-val (funcall key next))
                     (old-val (if accum
                                  (funcall key (car accum))
                                  zero)))
                 (cons (funcall combine
                                (funcall add-fun old-val key-val)
                                next)
                       accum)))
             seq
             :initial-value ()))))

(defun-ct over (fun &key (result-type 'list))
  (lambda (seq)
    (map result-type fun seq)))

(defun-ct on (fun key)
  (lambda (it)
    (funcall fun (funcall key it))))

(defpackage :zfs-cleaner
  (:use :cl :zfs-cleaner.utils))
(in-package :zfs-cleaner)

(defgeneric %get-snapshots (env)
  (:method ((env (eql :dev)))
    (alexandria:read-file-into-string #p "/tmp/feeds"))
  (:method ((env (eql :prod)))
    (with-output-to-string (s)
      (uiop:run-program "zfs list -r -t snapshot -H -p tank/feed_archive/feeds"
                        :output s))))

(defun get-snapshots (&optional (env :dev))
  (%get-snapshots env))

(shortcut find-date regex-match
  '(:group
    (:named-register "year" (:greedy-repetition 4 4 :digit-class)) #\-
    (:named-register "month" (:greedy-repetition 2 2 :digit-class)) #\-
    (:named-register "day" (:greedy-repetition 2 2 :digit-class)) #\-
    (:named-register "hour" (:greedy-repetition 2 2 :digit-class)) #\-
    (:named-register "minute" (:greedy-repetition 2 2 :digit-class))))

(defun-ct snapshot-to-vector (name)
  (map 'vector #'parse-integer
       (nth-value 1 (find-date name))))

(defun-ct vector-to-lt (vec)
  (apply 'local-time:encode-timestamp
         0 0
         (coerce (reverse vec)
                 'list)))

(defstruct (zfs-date (:type vector))
  year month day hour minute)

(defparameter +date-format+
  '((:year 4) #\-
    (:month 2) #\-
    (:day 2) #\-
    (:hour 2) #\-
    (:min 2)))

(defun get-snapshots-to-prune (snapshots)
  (labels ((first-column (it)
             (elt (fwoar.string-utils:split #\tab it)
                  0))
           (is-saved-snapshot (ts)
             (or (is-hourly-snapshot ts)
                 (not (is-stale ts))))
           (is-hourly-snapshot (it)
             (= (local-time:timestamp-minute it)
                0))
           (is-stale (item)
             (local-time:timestamp< item
                                    (local-time:timestamp- (local-time:now)
                                                           2 :day))))
    (funcall (alexandria:compose (pick #'second)
                                 (exclude (alexandria:compose (on #'is-saved-snapshot #'first)))
                                 (over (combine (alexandria:compose #'vector-to-lt
                                                                    #'snapshot-to-vector)
                                                #'first-column))
                                 (include 'find-date))
             (fwoar.string-utils:split #\newline snapshots))))

(defun dev-main ()
  (let ((snapshots (get-snapshots :dev)))
    (format t "~&~{~a~%~}" (get-snapshots-to-prune snapshots))))

(defun prod-main ()
  (let ((snapshots (get-snapshots :prod)))
    (format t "~&~{~a~%~}" (get-snapshots-to-prune snapshots))))
