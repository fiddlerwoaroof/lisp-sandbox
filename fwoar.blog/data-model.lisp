(in-package :fwoar.blog)

(defgeneric posts (blog)
  (:documentation "get a list of all the posts in the blog"))

(defgeneric content (post)
  (:documentation "get a post's content"))

(defgeneric title (post)
  (:documentation "get a post's title"))

;; constituent data-points

(defclass blog ()
  ((%posts :initarg :posts :accessor posts))
  (:default-initargs :posts ()))

(defclass post ()
  ((%content :initarg :content :accessor content)))

(defclass titled ()
  ((%title :initarg :title :accessor title)))

(defclass micropost (post)
  ()
  (:documentation "A tweet-style post"))

(defclass macropost (titled post)
  ()
  (:documentation "A longer post, with a title and content"))

;; protocols

(defgeneric titled-posts (blog)
  (:documentation "return blog posts that have titles")
  (:method ((blog blog))
    (mappend #'titled-posts
             (posts blog)))
  (:method ((post post))
    ())
  (:method ((post titled))
    (list post)))

(defgeneric find-post (slug blog)
  (:method ((slug string) (blog blog))
    (loop with needle-slug = (slugify slug)
          for post in (titled-posts blog)
          for haystack-slug = (slugify (title post))
          when (equal needle-slug haystack-slug)
            return post)))

(defmacro orc (&rest funs)
  "Run a bunch of functions against a value, stop when "
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

(make-constructor blog &rest posts)
(make-constructor micropost content)
(make-constructor macropost title content)

(defmethod print-object ((o blog) s)
  (format s "#.(make-blog ~{~s~^ ~})"
          (posts o)))

(defmethod print-object ((o micropost) s)
  (format s "#.(make-micropost ~s)" (content o)))

(defmethod print-object ((o macropost) s)
  (format s "#.(make-macropost ~s ~s)"
          (title o)
          (content o)))
