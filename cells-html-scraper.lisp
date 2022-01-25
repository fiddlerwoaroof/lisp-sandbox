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
  (defun request-new-items (interface page)
    (setf (cells-html-scraper::html (hnr-scraper interface))
          (drakma:http-request (format nil "https://news.ycombinator.com/~a" page))))

  (defun open-item (interface item)
    (capi:browser-pane-navigate (hnr-browser interface)
                                (puri:render-uri (url item)
                                                 nil)))
  
    
  
  
  (capi:define-interface hn-reader ()
    ((scraper :reader hnr-scraper :initform (make-instance 'hn-scraped)))
    (:panes
     (pages capi:list-panel
            :reader hnr-pages
            :items (list "news" "newest" "ask" "show" "jobs")
            :initial-constraints '(:visible-max-width (:string "newestest"))
            :selection-callback 'request-new-items
            :callback-type :interface-item)
     (item-panel capi:list-panel
                 :reader hnr-item-panel
                 :print-function 'title
                 :selection-callback 'open-item
                 :callback-type :interface-item)
     (browser capi:browser-pane
              :reader hnr-browser
              :url "https://fwoar.co"))
    (:layouts
     (right-side capi:column-layout
                 '(item-panel :divider browser)
                 :y-ratios '(1 nil 2)
                 :uniform-size-p nil)
     (main-layout capi:row-layout
                  '(pages :divider right-side)
                  :visible-min-width '(:character 120)
                  :visible-min-height '(:character 40)
                  :x-ratios '(1 nil 2)
                  :uniform-size-p nil))
    (:default-initargs
     :layout 'main-layout
     :title "HN Reader"))

  (defmethod initialize-instance :after ((o hn-reader) &key)
    (cells:defobserver
     cells-html-scraper::%items ((self (eql (hnr-scraper o))) new-value)
     
     (capi:apply-in-pane-process
      #1=(hnr-item-panel o)
      (lambda (pane)
        (let ((cleaned-items (remove-if 'null new-value :key 'cells-html-scraper::title)))
          (capi:remove-items pane (constantly t))
          (capi:append-items pane 
                             cleaned-items)))
      #1#))

    (setf (cells-html-scraper::html (hnr-scraper o))
          (drakma:http-request "https://news.ycombinator.com/")))
  (defun startup ()
    (capi:display (make-instance 'hn-reader)))
  )
