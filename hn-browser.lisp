(defpackage :fwoar.lisp-sandbox.hn-browser
  (:use :cl))
(in-package :fwoar.lisp-sandbox.hn-browser)

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
             (attr "href")))

(defun get-hn-data-cached ()
  (let ((a (load-time-value (list nil))))
    (if (car a)
        (car a)
      (setf (car a)
            (drakma:http-request "http://news.ycombinator.com")))))

(defun scrape-hn-page (text)
  (let* ((hnmain (lquery:$ (initialize text)
                   "#hnmain .itemlist tr.athing"
                   (combine (progn ".title a" (attr "href")
                              (node))
                            (progn ".title a" (text)
                              (node))
                            (hn-score)
                            (hn-age)
                            (hn-comments)))))
    (map 'vector
         (serapeum:op (apply 'make-hn-item _*))
         hnmain)))

(defclass hn-item ()
  ((%url :initarg :url :reader url)
   (%title :initarg :title :reader title)
   (%score :initarg :score :reader score)
   (%age :initarg :age :reader age)
   (%comments :initarg :comments :reader comments)))

(defun make-hn-item (url title score age comments)
  (flet ((normalize-uri (url)
           (puri:merge-uris (puri:parse-uri url)
                            "https://news.ycombinator.com")))
    (make-instance 'hn-item
                   :url (normalize-uri url)
                   :title title
                   :score (when score (parse-integer score :junk-allowed t))
                   :age age
                   :comments (when comments (normalize-uri comments)))))

(defclass hn-store ()
  ((%items :initarg :items :accessor items)
   (%selected-item-idx :initarg :selected :accessor selected-item-idx)
   (%url-type :accessor url-type :initform :|Article|)))

(fw.lu:defclass+ get-page ()
  ((%page-url :initarg :page-url :reader page-url)))

(fw.lu:defclass+ select-item ()
  ((%item :initarg :item :reader item)))

(fw.lu:defclass+ update-url-type ()
  ((%new-type :initarg :new-type :reader new-type)))

(defun selected-item (store)
  (elt (items store)
       (selected-item-idx store)))

(defun get-hn-data (suffix)
  (drakma:http-request (format nil "https://news.ycombinator.com/~a" suffix)))

(serapeum:defalias ui-data
  (data-lens:<>1
   (data-lens:transform-tail (data-lens:over (data-lens:applicable-when (lambda (it)
                                                                          (puri:render-uri it nil))
                                                                        (complement 'null))))
   (data-lens:juxt 'title 'url 'comments)))

(defgeneric apply-action (store action)
  (:method :around (store action)
   (prog1 store
     (call-next-method)))
  (:method ((store hn-store) (action null))
   (let ((hn-items (scrape-hn-page (get-hn-data-cached))))
     (setf (items store) hn-items
           (selected-item-idx store) 0)))
  (:method ((store hn-store) (action get-page))
   (let ((hn-items (scrape-hn-page (get-hn-data (page-url action)))))
     (setf (items store) hn-items)))
  (:method ((store hn-store) (action select-item))
   (let ((item (item action)))
     (setf (selected-item-idx store) (position item (items store)))))
  (:method ((store hn-store) (action update-url-type))
   (setf (url-type store) (new-type action))))

(defun request-new-items (interface page)
  (apply-action interface (get-page page)))

(defun open-item (interface item)
  (apply-action interface
                (select-item item)))
  
(defun switch-article-comments (item interface)
  (apply-action interface (update-url-type item)))
  
(capi:define-interface hn-reader (capi:interface hn-store)
  ()
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
   (browser-tabs capi:tab-layout
                 '(browser)
                 :selection-callback 'switch-article-comments
                 :items '(:|Article| :|Comments|))
   (right-side capi:column-layout
               '(item-panel :divider browser-tabs)
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

(defun refresh-items (interface)
  (let ((pane (hnr-item-panel interface)))
    (capi:apply-in-pane-process
     pane
     (lambda (pane)
       (let ((cleaned-items (remove-if 'null (items interface) :key 'title)))
         (capi:remove-items pane
                            (constantly t))
         (capi:append-items pane 
                            cleaned-items)))
     pane)))

(defmethod apply-action :after ((store hn-reader) (action null))
  (refresh-items store))

(defmethod apply-action :after ((store hn-reader) (action get-page))
  (refresh-items store))

(defmethod apply-action :after ((store hn-reader) (action update-url-type))
  (apply-action store (select-item (selected-item store))))

(defmethod apply-action :after ((store hn-reader) (action select-item))
  (let ((pane (hnr-browser store)))
    (capi:apply-in-pane-process
     pane
     (lambda (pane)
       ;;(format *debug-stream* "> ~{~s~^ ~}~%" (list item interface))
       (let ((current-item (capi:choice-selected-item (hnr-item-panel store)))
             (url-type (url-type store)))
         (ecase url-type
           (:|Article|
            (capi:browser-pane-navigate pane
                                        (second (ui-data (selected-item store)))))
           (:|Comments|
            (with-accessors ((comments comments)) current-item
              (when comments
                (capi:browser-pane-navigate pane
                                            (third (ui-data (selected-item store))))))))))
     pane)))

(defmethod initialize-instance :after ((o hn-reader) &key)
  (apply-action o nil))

(defun startup ()
  (capi:display (make-instance 'hn-reader)))
  