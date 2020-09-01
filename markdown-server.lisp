(defpackage :fwoar.markdown-server
  (:use :cl :fw.lu :fwoar.string-utils :alexandria :serapeum))
(in-package :fwoar.markdown-server)

(araneus:define-controller list-files (params)
  (declare (optimize (debug 3)))
  (uiop:directory-files (uiop:merge-pathnames* "onboarding/") "*.md"))

(araneus:define-view debug (model)
  (prin1-to-string model))

(araneus:define-spinneret-view list-files ((files list))
  (declare (optimize (debug 3)))
  (:html
   (:ul
    (loop for file in files
       do (:li (:a :href (format nil "/?file=~a" file)
		   (princ-to-string file)))))))

(araneus:define-controller markdown (params)
  (format t "~&~s~%" params)
  (let* ((specified-file (cdr (assoc "file" params :test 'equal)))
	 (source-file (uiop:merge-pathnames* specified-file)))
    (if (probe-file source-file)
	(cl-markdown:markdown (read-file-into-string source-file)
			      :format :none)
	(araneus::switch-view 'not-found))))

(araneus:define-spinneret-view not-found (model)
  (:body (:h1 "NOT FOUND")))

(defmethod araneus:view :around (name model)
  (spinneret:with-html-string
    (:html
     (:head
      (:link :rel "stylesheet" :href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css")
      (:link :rel "stylesheet" :href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css")
      (:style
       "body {"
       " width: 50%;"
       " max-width: 40em;"
       " margin-left: 10em;"
       " font-family: 'Lato', 'Helvetica', 'Arial', sans-serif;"
       " font-size: 20px;"
       "}"

       "ul {"
       " padding-left: 1em;"
       " background: rgba(0, 0, 128, 0.05);"
       "}"

       "li {"
       " list-style: circle outside;"
       " padding: 0.2em 0 0.2em 0.5em 0.1em;"
       " margin-left: 0.4em;"
       "}"

       "li:hover {"
       " background: rgba(0,0,64, 0.1);"
       "}"
       ))
     (:body
      (format spinneret:*html* (call-next-method))))))


(araneus:define-view markdown (model)
  (cl-markdown:render-to-stream model :html nil))

(defparameter *app* (make-instance 'ningle:<app>))
(araneus:defroutes *app*
  (("/index") (araneus:as-route 'list-files))
  (("/") (araneus:as-route 'markdown)))

(progn 
       (let ((handlers '()))
	 (defun start (&optional (port 9879))
	   (push (clack:clackup *app* :port port)
		 handlers))
	 (defun stop ()
	   (clack:stop (pop handlers)))))
