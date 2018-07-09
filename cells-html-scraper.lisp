(defpackage :cells-html-scraper
  (:use :cl :alexandria :serapeum :fw.lu :cells))

(in-package :cells-html-scraper)

(lquery:define-lquery-macro progn (nodes &rest args)
  `(lquery:$
     (inline ,nodes)
     ,@args))

(lquery:define-lquery-function hn-score (item)
  (lquery:$1 (inline item)
             (next)
             ".score"
             (text)))

(lquery:define-lquery-function hn-age (item)
  (lquery:$1 (inline item)
             (next)
             ".age"
             (text)))

(lquery:define-lquery-function hn-comments (item)
  (lquery:$1 (inline item)
             (next)
             ".age"
             (lquery-funcs:next)
             (next)
             (next)
             (text)))

(defclass hn-item ()
  ((%url :initarg :url :reader url)
   (%title :initarg :title :reader title)
   (%score :initarg :score :reader score)
   (%age :initarg :age :reader age)
   (%comments :initarg :comments :reader comments)))

(defun make-hn-item (url title score age comments)
  (make-instance 'hn-item
                 :url (puri:parse-uri url)
                 :title title
                 :score (when score (parse-integer score :junk-allowed t))
                 :age age
                 :comments (when comments (parse-integer comments :junk-allowed t))))

(defmodel hn-scraped ()
  ((%html :initarg :html
          :accessor html
          :initform (c-in ""))
   (%doc :reader %doc :initform (c? (plump:parse (^html))))
   (%hnmain :reader %hnmain
            :initform (c? (lquery:$1
                            (inline (^%doc))
                            "#hnmain")))
   (%body :reader %body
          :initform (c? (lquery:$
                          (inline (^%hnmain))
                          ".itemlist tr.athing")))
   (%titles :reader titles
            :initform (c? (lquery:$
                            (inline (^%body))
                            (combine (progn ".title .storylink" (attr "href")
                                            (node))
                                     (progn ".title .storylink" (text)
                                            (node))
                                     (hn-score)
                                     (hn-age)
                                     (hn-comments)))))
   (%items :reader items :initform (c? (map 'vector
                                            (op (apply 'make-hn-item _*))
                                            (^titles))))))

(defmodel url-getter ()
  ((%url :initarg :url
         :accessor url
         :initform (c-in '()))
   (%text :reader text
          :initform (c? (let ((drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
                          (drakma:http-request (^url)))))))

(defun get-links (url)
  (restart-case (values (map 'list (compose (op (list* url _))
                                            #'cdr)
                             (remove-if-not (op (string= _ "alternate"))
                                            (lquery:$
                                              (initialize (drakma:http-request url))
                                              "link"
                                              (combine (attr "rel") (attr "href") (attr "type")))
                                            :key #'car))
                        "")
    (continue nil
      :report (lambda (stream) (format stream "skip url ~a" url))
      (values nil url))))
