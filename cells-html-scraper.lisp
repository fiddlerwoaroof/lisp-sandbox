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
                            (combine (progn ".title a" (attr "href")
                                            (node))
                                     (progn ".title a" (text)
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

#+lispworks
(progn
  (defvar *item-panel* (make-instance 'capi:list-panel
                                      :print-function 'title
                                      :selection-callback 'open-item))
  (defvar *scraper* (make-instance 'hn-scraped))
  (defvar *browser-pane* nil)
  
  (defun request-new-items (page)
    (setf (cells-html-scraper::html *scraper*)
          (drakma:http-request (format nil "https://news.ycombinator.com/~a" page))))
  (defun open-item (item)
    (let ((was-bound-p *browser-pane*)
          (*browser-pane*
           (or *browser-pane*
               (setf *browser-pane*
                     (make-instance 'capi:browser-pane :debug t
                                    :initial-constraints '(:external-min-width (/ :screen-width 2)
                                                           :external-min-height :screen-height))))))
      (capi:browser-pane-navigate *browser-pane* (puri:render-uri (url item) nil))
      #+(or)
      (unless was-bound-p
        (capi:contain *browser-pane*))))
  (defvar *pages*
    (make-instance 'capi:list-panel
                   :items (list "news" "newest" "ask" "show" "jobs")
                   :initial-constraints '(:visible-max-width (:character 30))
                   :selection-callback 'request-new-items
                   :callback-type :item))
  
  (cells:defobserver cells-html-scraper::%items ((self (eql *scraper*)) new-value)
                     (map nil (lambda (it) (princ (cells-html-scraper::title it)) (terpri)) new-value)
                     (capi:apply-in-pane-process #1=*item-panel*
                                                 (lambda (pane)
                                                   (let ((cleaned-items (remove-if 'null new-value :key 'cells-html-scraper::title)))
                                                     (capi:remove-items pane (constantly t))
                                                     (capi:append-items pane 
                                                                        cleaned-items)))
                                                 #1#))
  (defun main-layout ()
    (let ((result (make-instance 'capi:row-layout
                                 :visible-min-width '(:character 120)
                                 :visible-min-height '(:character 40)
                                 :x-ratios '(1 nil 2)
                                 :uniform-size-p nil
                                 :children (list *pages* :divider
                                                 (make-instance 'capi:column-layout
                                                                :children (list *item-panel* :divider
                                                                                *browser-pane*))))))
      (prog1 result)))
  (capi:define-interface hn-reader ()
    (:panes )
                )
  )
