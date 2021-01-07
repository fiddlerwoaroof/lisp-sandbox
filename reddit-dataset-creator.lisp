(defpackage :fwoar.lisp-sandbox.reddit-dataset-creator
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.reddit-dataset-creator)

(defun get-data (subreddit)
  (alimenta:to-feed
   (fw.lu:closing
       (plump:parse
        (drakma:http-request (puri:merge-uris (format nil "~(~a~).rss?limit=100" subreddit)
                                              "https://www.reddit.com/r/")
                             :want-stream t)))))

(defun get-data-json (subreddit)
  (fw.lu:closing
      (yason:parse
       (drakma:http-request (puri:merge-uris (format nil "~(~a~).json?limit=100" subreddit)
                                             "https://www.reddit.com/r/")
                            :want-stream t))))

(defun subreddit-json-image-urls (subreddit &optional (fetch 'get-data-json-cached))
  (data-lens.transducers:transduce
   (data-lens:•
    (data-lens.transducers:mapping
     (data-lens:key "data"))
    #+nil
    (data-lens.transducers:mapping
     (lambda (it)
       (format t "~&~s~%" (gethash "url" it))
       it))
    (data-lens.transducers:mapping
     (data-lens:• (lambda (it)
                    (fw.lu:dive '("source" "url")
                                it))
                  (data-lens:applicable-when (data-lens:element 0)
                                             (complement #'null))
                  (data-lens:sorted '< :key (data-lens:key "width"))
                  (lambda (it)
                    (fw.lu:dive '("preview" "images")
                                it))))
    (data-lens.transducers:filtering 'identity))
   'data-lens.transducers:list-builder
   (fw.lu:dive '("data" "children")
               (funcall fetch subreddit))))

(defun subreddit-image-urls (subreddit &optional (fetch 'get-data-cached))
  (data-lens.transducers:transduce
   (data-lens:•
    (data-lens.transducers:mapping
     (data-lens:•
      (lambda (it)
        (lquery:$ (initialize it) "a" (attr "href")))
      'alimenta:content))
    (data-lens.transducers:catting)
    (data-lens.transducers:filtering
     (data-lens:regex-match "[.](jpe?g|png)$")))
   'data-lens.transducers:list-builder
   (funcall fetch subreddit)))

(defvar *reset-cache* nil)
(defun get-data-cached (subreddit)
  (let ((cache (load-time-value (make-hash-table))))
    (when *reset-cache*
      (clrhash cache)
      (setf *reset-cache* nil))
    (alexandria:ensure-gethash subreddit
                               cache
                               (get-data subreddit))))

(defun get-data-json-cached (subreddit)
  (let ((cache (load-time-value (make-hash-table))))
    (when *reset-cache*
      (clrhash cache)
      (setf *reset-cache* nil))
    (alexandria:ensure-gethash subreddit
                               cache
                               (get-data-json subreddit))))


(defun dump-stream (input output &optional fn)
  (with-open-stream (echo (make-echo-stream input output))
    (loop with buffer-length = 1000
          with buffer = (make-array buffer-length :element-type 'character)
          for read-chars = (read-sequence buffer echo)
          do (when fn
               (funcall fn buffer buffer-length))
             (unless (< read-chars buffer-length)
               (return nil)))))

(defun connect-streams (input output &key (background t) fn)
  "This reads from input and writes output until the end of input is found."
  (dump-stream input output fn))

(defun store-image (base category url)
  (let* ((uri (puri:parse-uri url))
         (fn (pathname-name
              (parse-namestring
               (puri:uri-path uri)))))
    (alexandria:with-output-to-file (s (ensure-directories-exist
                                        (merge-pathnames (make-pathname
                                                          :directory (list :relative
                                                                           (string category))
                                                          :name fn)
                                                         (parse-namestring base))))
      (fw.lu:closing
          (connect-streams (drakma:http-request url :want-stream t)
                           s
                           :background nil)))))

(format *standard-output* "~&~s ~s ~s ~s ~s ~s~%"
        SB-EXT:*CORE-PATHNAME*
        SB-EXT:*RUNTIME-PATHNAME*
        asdf/user::*nil-pathname*
        *COMPILE-FILE-PATHNAME*
        *LOAD-PATHNAME*
        swank/sbcl::*buffer-name*
        )

#+(or)
(asdf:initialize-source-registry
 (fwoar.git-systems:define-dir-deps ()
     (:git"alimenta"              "git@git.fiddlerwoaroof.com:u/edwlan/alimenta.git"              "master")
   (:git"alimenta-feed-archive" "git@git.fiddlerwoaroof.com:u/edwlan/alimenta-feed-archive.git" "master")
   (:git"data-lens"             "git@git.fiddlerwoaroof.com:data-lens.git"                      "master")
   (:git"collection-class"      "git@git.fiddlerwoaroof.com:u/edwlan/collection-class.git"      "master")))
