(defpackage :fwoar.blog
  (:use :cl :araneus :alexandria :serapeum :fw.lu)
  (:export ))
(in-package :fwoar.blog)

(defmacro new (class &rest initializer-syms)
  (multiple-value-bind (required optional rest) (parse-ordinary-lambda-list initializer-syms)
    (when optional
      (error "new doesn't handle optional arguments"))
    (if rest
        `(make-instance ,class
                        ,@(mapcan (serapeum:op (list (alexandria:make-keyword _1) _1))
                                  required)
                        ,(make-keyword rest) ,rest)
        `(make-instance ,class
                        ,@(mapcan (serapeum:op (list (alexandria:make-keyword _1) _1))
                                  initializer-syms)))))

(defun-ct %constructor-name (class)
  (format nil "~a-~a" '#:make class))

(defmacro make-constructor (class &rest args)
  (destructuring-bind (class &optional (constructor-name (intern (%constructor-name class))))
      (ensure-list class)
    `(defgeneric ,constructor-name (,@args)
       (:method (,@args)
         (new ',class ,@args)))))

(defclass blog ()
  ((%posts :initarg :posts :accessor posts))
  (:default-initargs :posts ()))
(make-constructor blog &rest posts)
(defmethod print-object ((o blog) s)
  (format s "#.(make-blog ~{~s~^ ~})"
          (posts o)))


(defclass post ()
  ((%content :initarg :content :accessor content)))

(defclass micropost (post)
  ())
(make-constructor micropost content)
(defmethod print-object ((o micropost) s)
  (format s "#.(make-micropost ~s)" (content o)))

(defclass macropost (post)
  ((%title :initarg :title :accessor title)))
(make-constructor macropost title content)
(defmethod print-object ((o macropost) s)
  (format s "#.(make-macropost ~s ~s)"
          (title o)
          (content o)))

(defmacro orc (&rest funs)
  `(lambda (v)
     (or ,@(loop for fun in funs
                 collect `(,fun v)))))

(defun slugify (string)
  (substitute #\- #\space
              (trim-whitespace
               (remove-if-not (orc alphanumericp
                                   (lambda (c) (eql c #\space))
                                   (lambda (c) (eql c #\-)))
                              (string-downcase string)))))

(defgeneric titled-posts (blog)
  (:method ((blog blog))
    (mappend #'titled-posts
             (posts blog)))
  (:method ((post micropost))
    ())
  (:method ((post macropost))
    (list post)))

(defgeneric find-post (slug blog)
  (:method ((slug string) (blog blog))
    (loop with needle-slug = (slugify slug)
          for post in (titled-posts blog)
          for haystack-slug = (slugify (title post))
          when (equal needle-slug haystack-slug)
            return post)))

(defclass blog-route ()
  ((%blog :initarg :blog :reader blog)))

(defclass index (blog-route)
  ())
(make-constructor (index make-blog-index))

(defclass post-route (blog-route)
  ((%post :initarg :post :reader post)))
(make-constructor (post make-blog-post))

(defmethod controller ((route index) params &key)
  (posts (blog route)))

(defmethod controller ((route post-route) params &key)
  (post route))

(defmethod view ((name post-route) (post macropost))
  (spinneret:with-html-string
    (:section
     (:h* (title post))
     (:div
      (content post)))))

(defmethod view ((name index) posts)
  (spinneret:with-html-string
    (:section
     (:h* "Blog Index")
     (:div
      (loop for post in posts
            do (call-current-view post))))))

(defmethod view ((name index) (post micropost))
  (spinneret:with-html
    (:section.post.micropost
     (content post))))


(defmethod view ((name index) (post macropost))
  (spinneret:with-html
    (:section.post.macropost
     (:h* (:a :href (format nil "/~a" (slugify (title post)))
              (title post))))))

(defun setup-routes (app blog)
  (defroutes app
    (("/" :method :GET) (as-route (make-instance 'index :blog blog)))
    (("/:post" :method :GET) (lambda (params)
                               (format t "~&params: ~s~%" params)
                               (let* ((post-name (cdr (assoc :post params)))
                                      (route (make-instance 'post-route :post (find-post post-name blog))))
                                 (run-route route params))))))

(defvar *blog*
  (make-blog (make-micropost "first post")
             (make-macropost "This is the title"
                             "This is the post content"))
  "The sample blog: passed lexically to the routes, so rebinding don't change nothin'")

(defun setup (&optional (blog *blog*))
  (prog1-bind (app (make-instance 'ningle:<app>))
    (setup-routes app blog)))

;;; entrypoint
(defvar *handler*)

(defun is-running ()
  (and (boundp '*handler*)
       *handler*))

(defun ensure-started (&rest r &key port)
  (declare (ignore port))
  (setf *handler*
        (if (not (is-running))
            (apply 'clack:clackup (setup) r)
            *handler*)))

(defun stop ()
  (if (is-running)
      (progn
        (clack:stop *handler*)
        (makunbound '*handler*)
        nil)
      nil))

#+fw.dev
(define-cluser-entrypoint (&optional (port 5000))
  (ensure-started :port port))
