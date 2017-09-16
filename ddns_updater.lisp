(eval-when (:load-toplevel :execute)
  (format t "~&Loading dependencies...")
  (ql:quickload '(:alexandria :serapeum :fwoar.lisputils :drakma :flexi-streams :osicat :yason)))

(eval-when (:load-toplevel :execute)
  (format t "~&Loading dependencies...")
  (osicat-posix:setenv "CC" "/usr/bin/gcc")
  (ql:quickload :net.didierverna.clon))

(defpackage :ddclient-updater
  (:use :cl :alexandria :serapeum :fw.lu)
  (:export :update-domains))
(in-package :ddclient-updater)

(defparameter *update-url* "https://api.1984.is/1.0/freedns/?apikey=~a&domain=~a&ip=")

(defvar *http-stream* nil)

(defun update-domain (domain api-key)
  (let* ((url (format nil *update-url* api-key domain))
	 (drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    ;; todo: we probably want to read the stream in, because yason isn't completely robust against early termination
    (multiple-value-bind (data _ __ ___ ____ stream) (drakma:http-request url :close nil :stream *http-stream*)
      (declare (ignore _ __ ___ ____))
      (unless *http-stream*
	(setf *http-stream* stream))
      (with (result (yason:parse data))
	(values result
		(gethash "ok" result)
		(gethash "msg" result))))))

(defun update-domains (domains api-key)
  (mapcar (op (with-simple-restart (continue "Skip ~a" _1)
		(format t "~&Updating ~a...~%" _1)
		(prog1 (multiple-value-list (update-domain _1 api-key))
		  (sleep 1))))
	  domains))

(defpackage :ddclient-updater.main
  (:use :cl :alexandria :serapeum :fw.lu :net.didierverna.clon :ddclient-updater))
(in-package :ddclient-updater.main)
(import 'ddclient-updater::*http-stream*)

(defparameter *api-key* "rKOB3TrfjWfsUl6NpvN6A3vYTaQfdXgYTShDAFWI5rwHJKwFb0EyBT7Mt11YWrjV")
(defparameter *domains* '("srv2.elangley.org" "vpn.elangley.org" "files.elangley.org" "home.elangley.org"
			  "mycloud.elangley.org" "pbj.elangley.org" "readme.elangley.org"
			  "wiki.elangley.org"))

(defsynopsis ()
  )
(defun main ()
  (make-context)
  (unwind-protect (update-domains *domains* *api-key*)
    (when *http-stream*
      (finish-output *http-stream*)
      (close *http-stream*))))

(defun dump-image ()
  (dump "ddns-updater" main))
