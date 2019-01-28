(defpackage :multi-fun
  (:shadow :rest)
  (:use :cl )
  (:export
   #:filter-functions
   #:number-recognizer
   #:delimited-field
   #:ignore-char
   #:whitespace
   #:iso-8601-timestamp
   #:parse-qs
   #:rest
   #:as
   #:splat-result
   #:defun
   #:month-recognizer
   #:take-field
   #:parse-format
   #:parse-file
   #:subformat
   #:treeify))
(in-package :multi-fun)

(defun filter-functions (funs list)
  (loop
     for fun in funs
     for results = (loop
                      for value in list
                      for (result . num) = (multiple-value-list (funcall fun value))
                      when result
                      collect (list (car num) result))
     when (and results (= (length results) (length list)))
     collect fun))

(defun number-recognizer (range)
  (destructuring-bind (min max)
      (etypecase range
        (integer (list 0 range))
        (list range))
    (flet ((in-range-p (n)
             (< min n (1+ max))))
      (lambda (str)
        (loop for digits
           from (min (length str)
                     (ceiling (log max 10)))
           downto (if (<= min 0)
                      1
                      (max 1
                           (floor (log min 10))))
           for (result . rest) = (multiple-value-list (parse-integer str :end digits :junk-allowed t))
           for relevant-result = (and result (in-range-p result))
           until relevant-result
           finally (when relevant-result
                     (return (values-list
                              (cons result rest)))))))))

(defun delimited-field (delimiter)
  (lambda (str)
    (alexandria:if-let ((val (position delimiter str)))
      (values (subseq str 0 val)
              (min (length str)
                   (1+ val)))
      (values str
              (length str)))))

(defun ignore-char (char)
  (lambda (str)
    (when (char= (elt str 0) char)
      (values 'drop 1))))

(defun whitespace ()
  (lambda (str)
    (values 'drop
            (position-if-not 'serapeum:whitespacep
                             str))))

(defun iso-8601-timestamp ()
  (lambda (str)
    (values (local-time:parse-timestring (subseq str 0 32))
            32)))

(defun parse-qs (str)
  (serapeum:mapply 'cons
                   (map 'list
                        (serapeum:op (coerce (fwoar.string-utils:split #\= _ :count 2) 'list))
                        (fwoar.string-utils:split #\& str))))

(defun rest (&optional (as #'identity))
  (lambda (str)
    (values (funcall as str)
            (length str))))

(defun as (fun transform)
  (flet ((do-transform (val) (funcall transform val)))
    (lambda (str)
      (fw.lu:transform-first-value (do-transform (funcall fun str))))))

(defun splat-result (fun)
  (flet ((splatize (val) (list* 'splat (alexandria:ensure-list val))))
    (lambda (str)
      (fw.lu:transform-first-value (splatize (funcall fun str))))))

(defun month-recognizer (str)
  (macrolet ((match-char (elt &body body)
               (let ((cases (mapcar (serapeum:op
                                      (if (listp _1)
                                          `(,(elt (car _1) elt) (match-char ,(1+ elt) ,@_1))
                                          `(,(elt _1 elt) (when (alexandria:starts-with-subseq ,_1 str)
                                                            ,_1))))
                                    body)))
                 `(when (> (length str) ,elt)
                    (case (elt str ,elt)
                      ,@cases)))))
    (alexandria:if-let ((value (match-char 0
                                           ("April" "August")
                                           "December"
                                           "February"
                                           ("January" ("July" "June"))
                                           (("March" "May"))
                                           "November"
                                           "October"
                                           "September")))
      (values value (length value))
      (values nil 0))))

(defun take-field (fun strs)
  (loop for str in strs
     for (val chars-read) = (multiple-value-list (funcall fun str))
     do (format t "~&VAL: ~s CHARS-READ: ~s" val chars-read)
     collect (list val (subseq str chars-read))))

(defun parse-format (funs str)
  (loop
     for line = str then (subseq line chars-read)
     for fun in funs
     for (val chars-read) = (multiple-value-list (funcall fun line))
     for total-chars = chars-read then (+ total-chars chars-read)
     if (and (consp val) (eql (car val) 'splat)) append (cdr val) into result
     else when (not (eql val 'drop)) collect val into result
     finally (return (values result
                             total-chars))))

(defmacro subformat ((v-sym &rest parsers) &body transform)
  `(flet ((transform (,v-sym)
            ,@(if transform
                  transform
                  (list v-sym))))
     (lambda (str)
       (fw.lu:transform-first-value (transform (parse-format (list ,@parsers) str))))))

(defun parse-file (format file &optional (get-record #'read-line))
  (let* ((records ())
         (cur records))
    (flet ((collect-record (record)
             (let ((new-cdr (list record)))
               (if cur
                   (setf (cdr cur) new-cdr)
                   (setf records new-cdr))
               (setf cur new-cdr))))
      (loop (let* ((line (funcall get-record file nil 'eof)))
              (when (eql line 'eof)
                (return records))
              (with-simple-restart (skip-record "Skip line ~s" line)
                (collect-record (coerce (parse-format format line)
                                        'vector))))))))

(defun treeify (strings)
  (declare (optimize (speed 3))
           (inline data-lens:over data-lens:transform-tail data-lens:applicable-when data-lens:of-min-length
                   data-lens:on data-lens:over data-lens:slice data-lens:compress-runs
                   data-lens:combine-matching-lists data-lens:juxt data-lens:element data-lens:sorted))
  (let* ((strip-prefixes
          (alexandria:compose
           (data-lens:over
            (data-lens:transform-tail 
             (data-lens:over
              (data-lens:transform-head (data-lens:slice 1)))))
           (data-lens:compress-runs
            :collector 'data-lens:combine-matching-lists)))
         (extract-keys-and-sort
          (alexandria:compose (data-lens:over
                               (data-lens:juxt (alexandria:compose
                                                (data-lens:element 0)
                                                (data-lens:element 0))
                                               'identity))
                              (data-lens:sorted 'char<
                                                :key (alexandria:compose
                                                      (data-lens:element 0)
                                                      (data-lens:element 0)))))
         (recurse
          (data-lens:over
           (data-lens:transform-tail
            (data-lens:applicable-when
             (lambda (x)
               (if (equal (caar x) "")
                   (cons (cons nil (cdar x))
                         (treeify (cdr x)))
                   (treeify x)))
             (data-lens:of-min-length 2)))))
         (step (data-lens:on strip-prefixes extract-keys-and-sort)))
    
    (funcall (alexandria:compose recurse step) strings)))




(defparameter +months+
  '("January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December"))
