(defpackage :fwoar.blog
  (:use :cl :araneus :alexandria :serapeum :fw.lu)
  (:export ))
(in-package :fwoar.blog)

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

(defclass index-route (blog-route)
  ())

(defclass post-route (blog-route)
  ((%post :initarg :post :reader post)))

(defmethod controller ((route index-route) params &key)
  (posts (blog route)))

(defmethod controller ((route post-route) params &key)
  (post route))

(defmethod view ((_ blog-route) post)
  '(404 (:content-type "text/plain") ("FAIL!")))

(defun layout (f)
  (spinneret:with-html-string
    (:style
     ":root {
        --zenburn-fg-plus-2: #FFFFEF; --zenburn-fg-plus-1: #F5F5D6;
        --zenburn-fg: #DCDCCC; --zenburn-fg-1: #A6A689;
        --zenburn-fg-2: #656555; --zenburn-black: #000000;
        --zenburn-bg-2: #000000; --zenburn-bg-1: #111112;
        --zenburn-bg-05: #383838; --zenburn-bg: #2A2B2E;
        --zenburn-bg-plus-05: #494949; --zenburn-bg-plus-1: #4F4F4F;
        --zenburn-bg-plus-2: #5F5F5F; --zenburn-bg-plus-3: #6F6F6F;
        --zenburn-red-plus-2: #ECB3B3; --zenburn-red-plus-1: #DCA3A3;
        --zenburn-red: #CC9393; --zenburn-red-1: #BC8383;
        --zenburn-red-2: #AC7373; --zenburn-red-3: #9C6363;
        --zenburn-red-4: #8C5353; --zenburn-red-5: #7C4343;
        --zenburn-red-6: #6C3333; --zenburn-orange: #DFAF8F;
        --zenburn-yellow: #F0DFAF; --zenburn-yellow-1: #E0CF9F;
        --zenburn-yellow-2: #D0BF8F; --zenburn-green-5: #2F4F2F;
        --zenburn-green-4: #3F5F3F; --zenburn-green-3: #4F6F4F;
        --zenburn-green-2: #5F7F5F; --zenburn-green-1: #6F8F6F;
        --zenburn-green: #7F9F7F; --zenburn-green-plus-1: #8FB28F;
        --zenburn-green-plus-2: #9FC59F; --zenburn-green-plus-3: #AFD8AF;
        --zenburn-green-plus-4: #BFEBBF; --zenburn-cyan: #93E0E3;
        --zenburn-blue-plus-3: #BDE0F3; --zenburn-blue-plus-2: #ACE0E3;
        --zenburn-blue-plus-1: #94BFF3; --zenburn-blue: #8CD0D3;
        --zenburn-blue-1: #7CB8BB; --zenburn-blue-2: #6CA0A3;
        --zenburn-blue-3: #5C888B; --zenburn-blue-4: #4C7073;
        --zenburn-blue-5: #366060; --zenburn-magenta: #DC8CC3;}"
     (lass:compile-and-write
      `(* :box-sizing border-box)
      `((:or html body)
        :margin 0
        :padding 0
        :color (var "--zenburn-fg")
        :background (var "--zenburn-bg")
        :font-family sans-serif
        :font-size 16px
        :display grid
        :grid-template-areas "\"top     top     top\"
                             \"left     main     main\"
                             \"left     main     main\"")
      `((:or a)
        :margin 0
        :padding 0
        :color (var "--zenburn-blue")
        )
      `((:or h1 h2 h3 h4 h5 h6)
        :font-size 1.1rem
        :margin 0
        :padding 0)
      `((:and a :hover)
        :color (var "--zenburn-blue-1"))
      `(h1 :grid-area "top" :padding 2rem :font-size 3rem)
      `(nav :grid-area "left" :padding 2rem)
      `(main :grid-area "main" :padding 2rem 0 0 0)))
    (:h* "the site!")
    (:nav "aside")
    (:main (funcall f))))

(defvar *layout-done* nil)
(defmethod view :around ((_ blog-route) post)
  (if *layout-done*
      (call-next-method)
      (let ((*layout-done* t))
        (layout #'call-next-method))))

(defmethod view ((name post-route) (post macropost))
  (spinneret:with-html
    (:section
     (:h* (title post))
     (:div
      (content post)))))

(defmethod view ((name index-route) posts)
  (format *standard-output* "~&~S~%" posts)
  (spinneret:with-html
    (:section
     (:h* "Blog Index")
     (:div
      (loop for post in posts
            do (call-current-view post))))))

(defmethod view ((name index-route) (post micropost))
  (spinneret:with-html
    (:section.post.micropost
     (content post))))


(defmethod view ((name index-route) (post macropost))
  (spinneret:with-html
    (:section.post.macropost
     (:h* (:a :href (format nil "/~a" (slugify (title post)))
              (title post))))))

(defun setup-routes (app blog)
  (defroutes app
    (("/" :method :GET)
     (as-route
      (make-instance 'index-route :blog blog)))
    (("/:post" :method :GET)
     (lambda (params)
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
