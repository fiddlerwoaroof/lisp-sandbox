(in-package :fwoar.blog)

(defclass blog-route ()
  ((%blog :initarg :blog :reader blog)))

(defclass index-route (blog-route)
  ())

(defclass post-route (blog-route)
  ((%post :initarg :post :reader post)))

(defmethod araneus:controller ((route index-route) action &key)
  (posts (blog route)))

(defclass save-action ()
  ((%post-type :initarg :post-type :reader post-type)
   (%post-initargs :initarg :post-initargs :reader post-initargs)))

(defmethod araneus:controller ((route post-route) (action (eql :view)) &key)
  (post route))
(defmethod araneus:controller ((route post-route) (action (eql :new)) &key)
  nil)
(defmethod araneus:controller ((route post-route) (action save-action) &key)
  (let ((blog (blog route)))
    (car
     (push (apply #'make-instance (post-type action) (post-initargs action))
           (posts blog)))))

(defun setup-routes (app blog)
  (araneus:defroutes app
    (("/" :method :GET)
     (lambda (_)
       (declare (ignore _))
       (let ((route (make-instance 'index-route :blog blog)))
         (araneus:run-route route :get))))

    (("/p/:post" :method :GET)
     (lambda (params)
       (format t "~&params: ~s~%" params)
       (let* ((post-name (cdr (assoc :post params)))
              (post (find-post post-name blog))
              (route (make-instance 'post-route :post post))
              (action (if post :view :new)))
         (araneus:run-route route action))))

    (("/micro" :method :POST)
     (lambda (params)
       (let* ((route (make-instance 'post-route :blog blog))
              (content (babel:octets-to-string (lack.request:request-content ningle:*request*))))
         (araneus:run-route route
                            (make-instance 'save-action
                                           :post-type 'micropost
                                           :post-initargs (list :content
                                                                (cdr (assoc "content" params
                                                                            :test 'equal))))))))))

