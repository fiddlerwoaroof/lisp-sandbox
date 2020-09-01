(in-package :fwoar.blog)

(defvar *blog*
  (make-blog (make-micropost "first post")
             (make-macropost "This is the title"
                             "This is the post content"))
  "The sample blog: passed lexically to the routes, so rebinding don't change nothin'")

(defparameter *acao-lens*
  (compose (data-lens.lenses:make-list-lens 1)
           (data-lens.lenses:make-plist-lens :Access-Control-Allow-Origin)))
(defparameter *acah-lens*
  (compose (data-lens.lenses:make-list-lens 1)
           (data-lens.lenses:make-plist-lens :Access-Control-Allow-Headers)))
(defparameter *acam-lens*
  (compose (data-lens.lenses:make-list-lens 1)
           (data-lens.lenses:make-plist-lens :Access-Control-Allow-Methods)))

(defun cors-middleware (app)
  (lambda (env)
    (if (eq :options
            (getf env :request-method))
        (rutilsx.threading:->>
         '(200 nil nil)
         (data-lens.lenses:set *acao-lens* "*")
         (data-lens.lenses:set *acah-lens* "Content-Type")
         (data-lens.lenses:set *acam-lens* "GET,POST,DELETE"))
        (let ((res (funcall app env)))
          (data-lens.lenses:set *acao-lens* "*"
                                res)))))

(defun setup (&optional (blog *blog*))
  (lack.builder:builder
   :accesslog
   'cors-middleware
   (prog1-bind (app (make-instance 'ningle:<app>))
     (setup-routes app blog))))

;;; entrypoint
(defvar *handler*)

(defun is-running ()
  (and (boundp '*handler*)
       *handler*))

(defun ensure-started (&rest r &key port)
  (declare (ignore port))
  (let ((app (setup)))
    (values app
            (setf *handler*
                  (if (not (is-running))
                      (apply 'clack:clackup app r)
                      *handler*)))))

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
