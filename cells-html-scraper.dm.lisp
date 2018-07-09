
(:DARKMATTER
 ((:ID . "1499699102316") (:NEXT . "1499699077360") (:PREV . "") (:COUNT . 0)
  (:LANG . "lisp")
  (:LISP . "(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (push (truename \"~/git_repos/fwoar.lisputils\") asdf:*central-registry*)
    (ql:quickload '(:alexandria :serapeum :lquery :cells :fwoar.lisputils :drakma :puri))))
(defpackage :cells-html-scraper
  (:use :cl :alexandria :serapeum :fw.lu :cells))
(in-package :cells-html-scraper)")
  (:MD . "")
  (:OUTPUT
   . "<div id=\"result\">#&lt;PACKAGE \"CELLS-HTML-SCRAPER\"&gt;</div>"))
 ((:ID . "1499699077360") (:NEXT . "1499699569637") (:PREV . "1499699102316")
  (:COUNT . 0) (:LANG . "lisp")
  (:LISP . "(lquery:define-lquery-macro progn (nodes &rest args)
  `(lquery:$
     (inline ,nodes)
     ,@args))

(lquery:define-lquery-function hn-score (item)
  (lquery:$1 (inline item)
             (next)
             \".score\"
             (text)))

(lquery:define-lquery-function hn-age (item)
  (lquery:$1 (inline item)
             (next)
             \".age\"
             (text)))

(lquery:define-lquery-function hn-comments (item)
  (lquery:$1 (inline item)
             (next)
             \".age\"
             (lquery-funcs:next)
             (next)
             (next)
             (text)))")
  (:MD . "") (:OUTPUT . "<div id=\"result\">HN-COMMENTS</div>"))
 ((:ID . "1499699569637") (:NEXT . "1499699613496") (:PREV . "1499699077360")
  (:COUNT . 0) (:LANG . "lisp")
  (:LISP . "(defclass hn-item ()
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
                 :comments (when comments (parse-integer comments :junk-allowed t))))")
  (:MD . "") (:OUTPUT . "<div id=\"result\">MAKE-HN-ITEM</div>"))
 ((:ID . "1499699613496") (:NEXT . "1499699601303") (:PREV . "1499699569637")
  (:COUNT . 0) (:LANG . "lisp")
  (:LISP . "(defmodel url-getter ()
  ((%url :initarg :url
         :accessor url
         :initform (c-in nil))
   (%text :initform (c? (if (^url)
                            (^url)
                            \"\"))
          :reader text)))")
  (:MD . "") (:OUTPUT . "<div id=\"result\">NIL</div>"))
 ((:ID . "1499699601303") (:NEXT . "1499699627294") (:PREV . "1499699613496")
  (:COUNT . 0) (:LANG . "lisp")
  (:LISP . "(defmodel hn-scraper ()
  ((%html :initarg :html
          :accessor html
          :initform (c-in \"\"))
   (%doc :reader %doc :initform (c? (plump:parse (^html))))
   (%hnmain :reader %hnmain
            :initform (c? (lquery:$1
                            (inline (^%doc))
                            \"#hnmain\")))
   (%body :reader %body
          :initform (c? (lquery:$
                          (inline (^%hnmain))
                          \".itemlist tr.athing\")))
   (%titles :reader titles
            :initform (c? (lquery:$
                            (inline (^%body))
                            (combine (progn \".title .storylink\" (attr \"href\")
                                            (node))
                                     (progn \".title .storylink\" (text)
                                            (node))
                                     (hn-score)
                                     (hn-age)
                                     (hn-comments)))))
   (%items :reader items :initform (c? (map 'vector
                                            (op (apply 'make-hn-item _*))
                                            (^titles))))))")
  (:MD . "") (:OUTPUT . "<div id=\"result\">NIL</div>"))
 ((:ID . "1499699627294") (:NEXT . "") (:PREV . "1499699601303") (:COUNT . 0)
  (:LANG . "lisp")
  (:LISP . "(defparameter *url-getter* (make-instance 'url-getter))
(defparameter *parser* (make-instance 'hn-scraper :html (^text *url-getter*)))")
  (:MD . "") (:OUTPUT . ""))) 