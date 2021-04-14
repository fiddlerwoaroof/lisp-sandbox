(defpackage :fwoar.lisp-sandbox.ical-parser
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.ical-parser)

(defvar *data* (alexandria:read-file-into-string "~/test.ics"))

(defun next-section (s &optional start)
  (if start
      (search "BEGIN" s :start2 start)
      (search "BEGIN" s)))

(defmacro str->stream ((op arg &rest r))
  (alexandria:once-only (arg)
    (alexandria:with-gensyms (s)
      `(with-input-from-string (,s ,arg)
         (,op ,s ,@r)))))

(defun get-line (stream)
  (loop for line = (read-line stream nil)
        while line
        collect line into results
        while (eql #\space (peek-char nil stream nil))
        finally (return (when results
                          (string-right-trim
                           '(#\newline #\return)
                           (serapeum:string-replace-all
                            #1=#.(coerce (list #\return #\space)
                                         'string)
                            (serapeum:string-join results "")
                            ""))))))

(defgeneric handle-begin (client block)
  (:method (_ block)
    (format t "~&;; >>>~a~%" block)))
(defgeneric handle-end (client block)
  (:method (_ block)
    (format t "~&;; <<<~a~%" block)))
(defgeneric handle-line (client tag params content)
  (:method (_ tag params content)
    (format t "~&;; - ~s•~s•~s~%" tag params content)))
(defun process-ics (client file)
  (let ((states '())
        (new-keywords '()))
    (unwind-protect
         (labels ((normalize (inp)
                    (multiple-value-bind (kw existing?)
                        (alexandria:make-keyword (string-upcase inp))
                      (unless existing?
                        (push kw new-keywords))
                      kw))
                  (%handle-block-delimiter (tag type)
                    (push type states)
                    (ecase tag
                      ((:begin) (handle-begin client type))
                      ((:end) (handle-end client type))))
                  (parse-params (inp)
                    (destructuring-bind (head params) (fwoar.string-utils:partition #\; inp)
                      (values head
                              (when params
                                (map 'list
                                     (data-lens:• (data-lens:transform-head #'normalize)
                                                  (serapeum:op
                                                    (fwoar.string-utils:partition #\= _)))
                                     (fwoar.string-utils:split #\; params))))))
                  (parse-property (it)
                    (destructuring-bind (s e) (fwoar.string-utils:partition #\: it)
                      (multiple-value-bind (head params) (parse-params s)
                        (list (normalize head)
                              params
                              e))))
                  (%handle-line (it)
                    (apply 'handle-line client it)))
           (with-input-from-string (s file)
             (loop for line = (get-line s)
                   for (%tag tagged) = (if line
                                           (fwoar.string-utils:partition #\: line)
                                           '(nil nil))
                   for tag = (fw.lu:may (normalize %tag))
                   while line
                   do (case tag
                        ((:begin)
                         (%handle-block-delimiter tag (normalize tagged)))
                        ((:end)
                         (%handle-block-delimiter tag (normalize tagged)))
                        (t (%handle-line (parse-property line)))))))
      (mapc 'unintern new-keywords))))

(fw.lu:defclass+ emit-sql ()
  ((%lines :accessor lines :initform ())
   (%tzid :accessor tzid :initform nil)
   (%db :reader db :initarg :db)))
(defmethod handle-begin ((client emit-sql) block)
  (values))
(defmethod handle-end :after ((client emit-sql) block)
  (setf (lines client) nil))
(defmethod handle-end :before ((client emit-sql) block)
  (values))
(defmethod handle-end ((client emit-sql) (block (eql :vevent)))
  (macrolet ((get-setter ()
               `(sxql:set= ,@(mapcan (lambda (it)
                                       (list it `(serapeum:assocadr
                                                  ,(alexandria:make-keyword
                                                    (substitute #\- #\_
                                                                (string it)))
                                                  (lines client))))
                                     '(:X_APPLE_TRAVEL_ADVISORY_BEHAVIOR
                                       :VEVENT :VALARM :RECURRENCE_ID
                                       :ORGANIZER :LAST_MODIFIED
                                       :EXDATE :CREATED :ATTENDEE
                                       :ATTENDEE :ATTACH :CATEGORIES
                                       :DESCRIPTION :DTEND :DTSTAMP
                                       :DTSTART :GEO :LOCATION :RRULE
                                       :SEQUENCE :STATUS :SUMMARY
                                       :TRANSP :UID :URL
                                       :X_ALT_DESC)))))
    (multiple-value-bind (query params)
        (sxql:yield
         (sxql:insert-into :vevent
           (get-setter)))
      (apply 'sqlite:execute-single
             (db client)
             query
             params))))
(defmethod handle-line ((client emit-sql) tag params content)
  (push (list tag content)
        (lines client)))

(defparameter +datetime-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; date yyyy-mm-dd
     (:REGISTER (:GREEDY-REPETITION 4 4 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     #\T
     ;; time hh-mm-ss
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     ;; tz
     (:GREEDY-REPETITION 0 1 (:REGISTER #\Z))
     :END-ANCHOR)))
(defparameter +date-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; date yyyy-mm-dd
     (:REGISTER (:GREEDY-REPETITION 4 4 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     :END-ANCHOR)))
(defparameter +time-scanner+
  (cl-ppcre:create-scanner
   '(:SEQUENCE
     :START-ANCHOR
     ;; time hh-mm-ss
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     (:REGISTER (:GREEDY-REPETITION 2 2 :DIGIT-CLASS))
     ;; tz
     (:GREEDY-REPETITION 0 1 (:REGISTER #\Z))
     :END-ANCHOR)))

(defparameter +sqlite-format+
  '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2)
    #\space (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)
    :GMT-OFFSET-OR-Z))

(defun parse-datetime (time timezone)
  (trivia:ematch (nth-value 1 (cl-ppcre:scan-to-strings +datetime-scanner+ time))
    (#(ye mo da ho mi se tz)
      (local-time:encode-timestamp
       0
       (fw.lu:if-let* ((se (parse-integer se))
                       (_ (= 60 se)))
         59
         se)
       (parse-integer mi) (parse-integer ho)
       (parse-integer da) (parse-integer mo) (parse-integer ye)
       :timezone (if (equal tz "Z")
                     local-time:+utc-zone+
                     timezone)))))

(defun parse-date (time)
  (trivia:ematch (nth-value 1 (cl-ppcre:scan-to-strings +date-scanner+ time))
    (#(ye mo da)
      (local-time:encode-timestamp
       0 0 0 0
       (parse-integer da) (parse-integer mo) (parse-integer ye)))))

;;what do I do for the date here???
#+(or)
(defun parse-time (time)
  (trivia::match (nth-value 1 (cl-ppcre:scan-to-strings +time-scanner+ time))
    (#(ho mi se)
      (local-time:make-timestamp
       0 0 0 0
       (parse-integer da) (parse-integer mo) (parse-integer ye)))))

(defun handle-ical-date (client tag params content)
  (push (list tag
              (local-time:format-timestring
               nil
               (case (alexandria:make-keyword
                      (string-upcase
                       (serapeum:assocadr :value params)))
                 (:date (parse-date content))
                 (t (parse-datetime content
                                    (or (local-time:find-timezone-by-location-name
                                         (serapeum:assocadr :tzid params))
                                        (tzid client)))))
               :format +sqlite-format+))
        (lines client)))

(defmethod handle-line ((client emit-sql) (tag (eql :dtstart)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-line ((client emit-sql) (tag (eql :dtend)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-line ((client emit-sql) (tag (eql :created)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-line ((client emit-sql) (tag (eql :dtstamp)) params content)
  (handle-ical-date client tag params content))
(defmethod handle-line :after ((client emit-sql) (tag (eql :dtstart)) params content)
  (format t "~&~s ~s~%~4t~s~%~4t~s~%" tag params
          content
          (parse-time content
                      (or (local-time:find-timezone-by-location-name
                           (serapeum:assocadr :tzid params))
                          (tzid client)))))

(defun setup-sql ()
  (sxql:yield
   (sxql:create-table (:vevent :if-not-exists t)
       ((:ATTACH :type 'text)
        (:ATTENDEE :type 'text)
        (:CATEGORIES :type 'text)
        (:CREATED :type 'text)
        (:DESCRIPTION :type 'text)
        (:DTEND :type 'text)
        (:DTSTAMP :type 'text)
        (:DTSTART :type 'text)
        (:EXDATE :type 'text)
        (:GEO :type 'text)
        (:LAST_MODIFIED :type 'text)
        (:LOCATION :type 'text)
        (:ORGANIZER :type 'text)
        (:RECURRENCE_ID :type 'text)
        (:RRULE :type 'text)
        (:SEQUENCE :type 'text)
        (:STATUS :type 'text)
        (:SUMMARY :type 'text)
        (:TRANSP :type 'text)
        (:UID :type 'text)
        (:URL :type 'text)
        (:VALARM :type 'text)
        (:VEVENT :type 'text)
        (:X_ALT_DESC :type 'text)
        (:X_APPLE_TRAVEL_ADVISORY_BEHAVIOR :type 'text))
     (sxql:primary-key '(:sequence :uid :recurrence_id)))))

(defun ics->sqlite (fn data)
  (sqlite:with-open-database (db fn)
    (sqlite:execute-non-query
     db (sxql:yield (sxql:drop-table :vevent :if-exists t)))
    (sqlite:execute-non-query
     db (setup-sql))
    (sqlite:with-transaction db
      (process-ics (emit-sql db) data))))


(fw.lu:defclass+ build-tree ()
  ((%history :accessor history :initform ())))

(defmethod handle-begin ((client build-tree) block)
  (push (list block) (history client)))
(defmethod handle-end ((client build-tree) block)
  (progn (when (cdr (history client))
           (let ((last (pop (history client))))
             (push (nreverse last)
                   (car (history client)))))))
(defmethod handle-line ((client build-tree) tag params content)
  (push (list tag params content)
        (car (history client))))

(defun ics->tree (data)
  (let ((client (build-tree)))
    (process-ics client data)
    (nreverse (car (history client)))))

(fw.lu:defclass+ one-vevent ()
  ((%started :accessor started :initform nil)
   (%lines :accessor lines :initform nil)))

(defmethod handle-begin ((client one-vevent) block)
  (when (eql block :vevent)
    (push block (started client))))

(defmethod handle-line ((client one-vevent) tag params content)
  (when (started client)
    (push (list tag params content)
          (lines client))))

(defmethod handle-end ((client one-vevent) block)
  (when (eql block :vevent)
    (throw 'vevent
      (nreverse (lines client)))))

(defun extract-one-vevent (data)
  (catch 'vevent
    (process-ics (one-vevent) data)))
