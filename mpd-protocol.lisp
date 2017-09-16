(defpackage :mpd-protocol
  (:use :cl :alexandria :serapeum :esrap)
  (:shadowing-import-from :string-case :string-case))

(in-package :mpd-protocol)

(progn (defparameter *the-sock* (usocket:socket-connect "127.0.0.1" 6600))
       (get-line))

(defparameter *buffer* (make-array 100 :element-type '(unsigned-byte 8)))

(defun get-line ()
  (read-line (usocket:socket-stream *the-sock*) nil))

(defun send-command (command &rest args)
  (write-line (string-join (list* command args) #\space)
	      (usocket:socket-stream *the-sock*))
  (finish-output (usocket:socket-stream *the-sock*))
  (loop for line = (get-line)
     while (and line (string/= line "OK"))
       collect line))
