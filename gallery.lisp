#.(progn
    (asdf:defsystem :fwoar.gallery
      :depends-on (:lquery
                   :araneus
                   :drakma
                   :clack
                   :yason
                   :ningle
                   :lass))
    (ql:quickload :fwoar.gallery)
    nil)

(defpackage :fwoar.gallery
  (:use :cl :araneus :alexandria :serapeum)
  (:export ))
(in-package :fwoar.gallery)

(defvar *url*)
(defvar *gallery*)

(defmacro new (class &rest initializer-syms)
  `(make-instance ,class
                  ,@(mapcan (serapeum:op (list (alexandria:make-keyword _1) _1))
                            initializer-syms)))

(defclass gallery ()
  ((%images :initarg :images :initform () :reader images)))

(defun make-gallery (images)
  (new 'gallery images))

(defclass page ()
  ((%gallery :initarg :gallery :reader gallery)
   (%page :initarg :page :initform 1 :reader page)))

(defun make-page (gallery page)
  (new 'page gallery page))

(defclass image ()
  ((%url :initarg :url :initform (error "need url for image") :reader url)))
(defclass video ()
  ((%url :initarg :url :initform (error "need url for video") :reader url)))

(defmethod print-object ((o image) s)
  (format s "#.(make-image \"~a\")" (url o)))
(defmethod print-object ((o video) s)
  (format s "#.(make-video \"~a\")" (url o)))
(defmethod print-object ((o gallery) s)
  (if *print-readably*
      (format s "#.(make-gallery (list ~{~a~^~%~22t~}))" (images o))
      (format s "#.(make-gallery (~a images))" (length (images o)))))

(defun make-image (url)
  (new 'image url))
(defun make-video (url)
  (new 'video url))

(defun page->gallery (dom base-url)
  (check-type dom plump:node)
  (make-gallery
   (coerce (lquery:$ 
             (inline dom)
             "a" 
             (attr "href") 
             (filter (lambda (u) (search "jpg" u)))
             (filter (complement (lambda (u) (search "thumb" u))))
             (map (lambda (u)
                    (with-output-to-string (s)
                      (puri:render-uri
                       (puri:merge-uris u base-url)
                       s))))
             (map 'make-image))
           'list)))


(define-controller root (params gallery)
  (let ((page (make-page gallery
                         (parse-integer
                          (or (cdr (assoc "page" params
                                          :test #'equalp))
                              "1")))))
    page))

(defmacro hostname-case (uri &body cases)
  `(serapeum:string-case (puri:uri-host ,uri)
     ,@cases))

(defun get-site (uri)
  (hostname-case uri
    ("gfycat.com" :gfycat)
    ("imgur.com" :imgur)
    ("i.redd.it" :ireddit)
    ("v.redd.it" :vreddit)))

(defgeneric transform-url (site uri)
  (:method (_ uri)
    (declare (ignore _))
    (make-image (puri:render-uri uri nil)))
  (:method ((site (eql :gfycat)) uri)
    (make-video
     (format nil "~a"
             (puri:render-uri
              (fw.lu:prog1-bind (uri (puri:copy-uri uri))
                (setf (puri:uri-host uri) "gfycat.com"
                      (puri:uri-path uri) (format nil "/ifr~a" (puri:uri-path uri))))
              nil))))
  (:method ((site (eql :imgur)) uri)
    (make-image
     (format nil "~a.jpg"
             (puri:render-uri
              (fw.lu:prog1-bind (uri (puri:copy-uri uri))
                (setf (puri:uri-host uri) "i.imgur.com"))
              nil))))
  (:method ((site (eql :vreddit)) uri)
    (make-video (puri:render-uri
                 (fw.lu:prog1-bind (uri (puri:copy-uri uri))
                   (setf (puri:uri-path uri)
                         (format nil "~a/HLSPlaylist.m3u8"
                                 (puri:uri-path uri))))
                 nil))))

(defun ensure-uri (uri)
  (etypecase uri
    (string (puri:parse-uri uri))
    (puri:uri uri)))

(defun process-uri-list (uris)
  (loop for raw-uri in uris
        for uri = (ensure-uri raw-uri)
        for transformed = (transform-url (get-site uri) uri)
        when transformed
          collect transformed))

(spinneret:deftag css (body attrs)
  `(:style :type "text/css"
           ,@attrs
           (:raw
            ,(format nil "~%")
            ,(apply 'lass:compile-and-write
                    body)
            )))

(spinneret:deftag js (body attrs)
  `(:script :type "text/javascript"
            ,@attrs
            (:raw
             ,(format nil "~%")
             (ps:ps
               ,@body))))

(defun gallery-css ()
  (spinneret:with-html
    (css
      (:let ((bottom-gap 2em)
             (gallery-height (calc (- 100vh 2em))))
        (*
         :box-sizing border-box)
        (html
         :outline "thin solid black")
        ((:or body html div section)
         :margin 0
         :padding 0)

        (.gallery
         :display block
         :background "#888"
         :height #(gallery-height)
         :overflow-y scroll
         :scroll-snap-type "y mandatory"

         :flex-wrap wrap)

        ((.gallery img)
         :width 100%
         :height 100%
         :scroll-snap-align start
         :object-fit contain)
        ((.gallery video)
         :width 100%
         :height 100%
         :scroll-snap-align start
         :object-fit contain)
        ((.gallery iframe)
         :width 100%
         :height 100%
         :scroll-snap-align start
         :object-fit contain)
        ))))

(defun gallery-js ()
  (js
   (ps:chain #() for-each
             (call (ps:chain document
                             (query-selector-all ".gallery > div"))
                   (lambda (it)
                     (ps:chain it (add-event-listener
                                   "click"
                                   (lambda ()
                                     (ps:chain #() for-each
                                               (call (ps:chain document (query-selector-all ".expanded"))
                                                     (lambda (other)
                                                       (unless (eql other it)
                                                         (ps:chain other class-list (remove "expanded"))))))
                                     (ps:chain it class-list (toggle "expanded"))))))))) )


(defmethod view :around ((name (eql 'root)) (model page))
  (spinneret:with-html-string
    (:html
     (:head
      (gallery-css))
     (:body
      (let ((*gallery* (make-gallery (subseq (images (gallery model))
                                             (* 52 (1- (page model)))
                                             (min (* 52 (page model))
                                                  (length (images (gallery model))))))))
        (call-next-method))
      (when (<= (* 52 (page model))
                (length (images (gallery model))))
        (:a :href (format nil "/?page=~d" (1+ (page model)))
            :style "width: 100%; text-align: center; display: inline-block;"
            "next"))
      (gallery-js)))))

(define-view root ((model page))
  (spinneret:with-html
    (:section.gallery
     (loop for img in (images *gallery*)
           do (call-current-view img)))))

(define-view root ((model image))
  (spinneret:with-html
    (:img :src (url model))))

(define-view root ((model video))
  (spinneret:with-html
    (if (string-contains-p "/ifr/" (url model))
        (:iframe :src (url model))
        (:video :autoplay "autoplay"
                :loop "loop"
                (:source :src (url model)
                         :type "video/mp4")))))

(defun initialize-app (app gallery)
  (defroutes app
    (("/") (as-route 'root :gallery gallery))))

(defun get-reddit-items (r)
  (process-uri-list
   (mapcar (lambda (i)
             (fw.lu:dive '("data" "url") i))
           (fw.lu:dive '("data" "children") r))))

(defun main (url)
  (let* ((app (make-instance 'ningle:<app>))
         (gallery (page->gallery (plump:parse
                                  (babel:octets-to-string (drakma:http-request url :force-binary t)
                                                          :encoding :latin-1))
                                 url)))
    (initialize-app app gallery)
    (clack:clackup app)))

(defun reddit-main (subreddits)
  (let* ((app (make-instance 'ningle:<app>))
         (gallery (make-gallery
                   (mapcan
                    (lambda (subreddit)
                      (get-reddit-items
                       (yason:parse
                        (babel:octets-to-string
                         (drakma:http-request (format nil "https://reddit.com/r/~a.json" subreddit)
                                              :force-binary t)
                         :encoding :latin-1))))
                    subreddits))))
    (setf *gallery* gallery)
    (initialize-app app gallery)
    (clack:clackup app)))

(defun cl-user::fwoar.gallery.main (version init)
  (ecase version
    (:reddit (reddit-main init))
    (:dir (main init))))
