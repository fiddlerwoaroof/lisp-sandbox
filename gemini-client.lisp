(defpackage :gemini.client
  (:use :cl))
(in-package :gemini.client)

(defun ssl-connection (host port)
  (flexi-streams:make-flexi-stream
   (comm:open-tcp-stream host port :ssl-ctx t)
   :external-format :utf-8))

(defun crlf (s)
  (princ #\return s)
  (princ #\newline s))

(defun request (uri s)
  (puri:render-uri uri s)
  (crlf s))

(fw.lu:defclass+ link ()
  ((url :initarg :url :reader url)
   (text :initarg :text :reader text))))
(defmethod print-object ((it link) s)
  (print-unreadable-object (it s :type t :identity t)
    (format s "target: ")
    (puri:render-uri (url it) s))

(defun parse-link (base-uri line)
  (let* ((raw (subseq line 2))
         (raw (serapeum:trim-whitespace raw))
         (split (split-sequence:split-sequence-if #'serapeum:whitespacep raw)))
    (destructuring-bind (first . rest) split
      (link (puri:merge-uris (puri:uri first)
                             base-uri)
            (when rest (car rest))))))

(defun gemini (uri s)
  (let* ((uri (puri:uri uri))
         (buffer (make-string 1024 :element-type 'character)))
    (with-open-stream (conn (ssl-connection (puri:uri-host uri) 1965))
      (request uri conn)
      (finish-output conn)
      (loop for read = (read-sequence buffer conn)
            while (= read (length buffer))
            do (write-sequence buffer s)
            finally (write-sequence buffer conn :end read))
      uri)))

(defun gemini-byline (uri s)
  (let* ((uri (puri:uri uri))
         (links ()))
    (with-open-stream (conn (ssl-connection (puri:uri-host uri) 1965))
      (request uri conn)
      (finish-output conn)
      (loop with preformatted = nil
            for read = (read-line conn nil)
            while read
            do
            (write-sequence read s)
            (terpri s)
            when (equal read "```") do (setf preformatted (not preformatted))
            when (and (not preformatted) (> (length read) 2) (equal (subseq read 0 2) "=>"))
            do (push (parse-link uri read) links))
      (values uri links))))
