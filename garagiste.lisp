(defpackage :garagiste-app
  (:use :cl )
  (:export ))
(in-package :garagiste-app)

(defclass recording-datastore ()
  ((%record :accessor record
            :initform (make-array 10
                                  :adjustable t
                                  :fill-pointer 0))))
(defclass jsonapi-datastore ()
  ((%database :reader database
              :initform (make-hash-table))))
(defgeneric add-object (store object))
(defgeneric objects-of-type (store type))
(defgeneric get-object (store type id))
(defgeneric ingest-jsonapi (store object))
(defgeneric stored-types (store))

(defun object (store type id)
  (get-object store type id))

(defun (setf object) (new-value store type id)
  (let ((tmp (alexandria:copy-hash-table new-value)))
    (setf (gethash "id" tmp) id
          (gethash "type" tmp) type)
    (add-object store tmp)))

(defclass batch ()
  ((%id :accessor id)
   (%shipments :accessor shipments-a)))
(defmethod add-object ((store jsonapi-datastore) (object batch))
  (setf (object store :batches (id object))
        (alexandria:alist-hash-table
         `(("relationships" . ,(mapcar (lambda (it)
                                         (alexandria:alist-hash-table
                                          `(("type" . "shipments")
                                            ("id" . ,it))))
                                       (shipments-a object)))))))

(defun normalize-type (type)
  (declare (optimize debug))
  (let ((normalize-char (lambda (ch)
                          (case ch
                            (#\_ #\-)
                            (t (if (lower-case-p ch)
                                   (char-upcase ch)
                                   (char-downcase ch)))))))
    (nth-value
     0 (etypecase type
         (simple-string (intern (map 'simple-string
                                     normalize-char
                                     type)
                                :keyword))
         (string (intern (map 'simple-string
                              normalize-char
                              type)
                         :keyword))
         (keyword type)))))

(defmacro let-lazy ((&rest bindings) &body body)
  (let* ((cache-syms (mapcar (lambda (it)
                               (list (gensym (symbol-name (first it)))
                                     nil))
                             bindings))
         (cache-bindings (mapcar (lambda (sym cache-sym)
                                   `(,(first sym) (or ,(first cache-sym)
                                                      (setf ,(first cache-sym)
                                                            ,(second sym)))))
                                 bindings
                                 cache-syms)))
    `(let ,cache-syms
       (declare (ignorable ,@(mapcar 'first cache-syms)))
       (symbol-macrolet ,cache-bindings
         ,@body))))

(defmacro hash-table-dbind (bindings h-t &body body)
  (alexandria:once-only (h-t)
    `(let-lazy ,(loop for (name key . rest) in bindings
                      collect `(,name (gethash ,key ,h-t ,@rest)))
               ,@body)))

(defmacro destructure-resource (resource &body body)
  `(hash-table-dbind ((id "id")
                      (type "type")
                      (attributes "attributes" 'empty)
                      (relationships "relationships" 'empty)
                      (links "links" 'empty)
                      (meta "meta" 'empty))
                     ,resource
                     ,@body))

(defmethod objects-of-type ((store jsonapi-datastore) type)
  (gethash (normalize-type type)
           (database store)))

(defgeneric id-resolver-gf (store resource))
(defmethod id-resolver-gf ((store jsonapi-datastore) (resource hash-table))
  (destructure-resource resource
                        (get-object store type id)))
(defmethod id-resolver-gf ((store jsonapi-datastore) (resource string))
  (patmatch:let-pat* (((vector type id) (fwoar.string-utils:split "/" resource :count 1)))
                     (get-object store
                                 type
                                 id)))

(defun id-resolver (store)
  (lambda (resource)
    (id-resolver-gf store resource)))

(defmethod add-object :before ((store recording-datastore) (object hash-table))
  (vector-push-extend (destructure-resource object
                                            (list :add-object id type attributes))
                      (record store)))

(defmethod stored-types ((store jsonapi-datastore))
  (alexandria:hash-table-keys (database store)))


(defmethod ingest-jsonapi ((store jsonapi-datastore) (object hash-table))
  (hash-table-dbind ((data "data" 'empty)
                     (errors "errors" 'empty)
                     (meta "meta" 'empty)
                     (jsonapi "jsonapi" 'empty)
                     (links "links" 'empty)
                     (included "included" 'empty))
                    object
                    (when (and (eql data 'empty)
                               (eql errors 'empty)
                               (eql meta 'empty))
                      (warn "one of data, errors or meta must be specified in a toplevel object"))
                    (when (not (or (eql data 'empty)
                                   (eql errors 'empty)))
                      (warn "only one of data or errors may be specified"))
                    (unless (when (eql data 'empty)
                              (not (eql included 'empty)))
                      (warn "included may only be specified if data is specified"))
                    (if (typep included 'sequence)
                        (add-object store included)
                        (unless (eql included 'empty)
                          (warn "included should be a sequence")
                          (add-object store included)))
                    (add-object store data)))

(defun resource-identifier-p (resource)
  (destructure-resource resource
                        (and resource
                             (eql attributes 'empty)
                             (eql relationships 'empty)
                             (eql links 'empty))))

(defmacro with-json-output ((stream &key indent) &body body)
  "Set up a JSON streaming encoder context on STREAM, then evaluate BODY."
  `(let* ((yason::*json-output*
            (make-instance 'yason::json-output-stream
                           :output-stream ,stream
                           :indent ,indent))
          (,stream yason::*json-output*))
     ,@body))

(defmethod add-object ((store jsonapi-datastore) (object hash-table))
  (destructure-resource object
                        (let* ((store-ht (database store))
                               (type-store (alexandria:ensure-gethash (normalize-type type)
                                                                      store-ht
                                                                      (make-hash-table :test 'equal)))
                               (old-value (gethash id type-store)))
                          (unless (eql relationships 'empty)
                            (loop for v being the hash-values of relationships
                                  for data = (gethash "data" v)
                                  when data do
                                    (add-object store data)))
                          (if (resource-identifier-p old-value)
                              (setf (gethash id type-store) object)
                              (if (resource-identifier-p object)
                                  old-value
                                  (setf (gethash id type-store) object))))))

(defmethod add-object ((store jsonapi-datastore) (objects sequence))
  (map 'list
       (lambda (it)
         (add-object store it))
       objects))

(defmethod get-object ((store jsonapi-datastore) type id)
  (let* ((store-ht (database store))
         (type-store (alexandria:ensure-gethash (normalize-type type)
                                                store-ht
                                                (make-hash-table :test 'equal))))
    (gethash id type-store)))

(defun summarize-objects-of-type (store type map-fn filter-fn)
  (serapeum:collecting
   (serapeum:do-hash-table (k v (gethash type (database store)))
     (declare (ignore k))
     (when (funcall filter-fn v)
       (collect (funcall map-fn v))))))

(defun get-signin-page ()
  (let ((c-j (make-instance 'drakma:cookie-jar)))
    (values (plump:parse
             (drakma:http-request "https://app.garagiste.com/users/sign_in"
                                  :cookie-jar c-j))
            c-j)))

(defun extract-auth-token (doc)
  (lquery:$1 (inline doc)
             "form input[type=hidden][name=authenticity_token]"
             (attr "value")))

(defvar *csrf-token* nil)
(defun request-json (url cj &rest r &key (method :get) (parameters ()))
  (declare (ignore method parameters))
  (let ((drakma:*text-content-types* (acons "application" "json"
                                            drakma:*text-content-types*))
        (yason:*parse-json-booleans-as-symbols* t)
        #+(or)(yason:*parse-json-null-as-keyword* t)
        (yason:*parse-json-arrays-as-vectors* t))
    #+(or)identity
    (yason:parse
     (apply #'drakma:http-request url
            :cookie-jar cj
            :accept "application/json"
            :additional-headers (when *csrf-token*
                                  `(("X-CSRF-Token" . ,*csrf-token*)))
            r))))

(defun login-garagiste (auth-token cj)
  (drakma:http-request "https://app.garagiste.com/users/sign_in"
                       :cookie-jar cj
                       :method :post
                       :parameters `(("utf8" . ,(drakma:url-encode "✓" :utf-8))
                                     ("authenticity_token" . ,auth-token)
                                     ("user[email]" . ,(swank:ed-rpc
                                                        'get-passwd :garagiste-email
                                                        "Garagiste Email? "))
                                     ("user[password]" . ,(swank:ed-rpc
                                                           'get-passwd :garagiste
                                                           "Garagiste Password? "))
                                     ("user[remember_me]" . "0")
                                     ("commit" . "Log in"))))


(defun get-user-data ()
  (multiple-value-bind (doc cookie-jar) (get-signin-page)
    (let* ((auth-token (extract-auth-token doc))
           (user-home (login-garagiste auth-token cookie-jar)))
      (setf *csrf-token*
            (lquery:$1 (initialize user-home)
                       "meta[name=csrf-token]"
                       (attr "content")))
      (values (yason:parse
               (lquery:$1 (initialize user-home)
                          "div[data-react-class=Portal]"
                          (attr "data-react-props")))
              cookie-jar))))

(defun user-id (ud)
  (fw.lu:dive '("data" "customerId")
              ud))

(defun customer (user-id cookie-jar)
  (request-json (format nil "https://app.garagiste.com/customers/~a" user-id)
                cookie-jar))

(defun requests (user-id cookie-jar)
  (request-json (format nil "https://app.garagiste.com/customers/~a/requests/" user-id)
                cookie-jar))

(defun offerings (user-id cookie-jar)
  (offering-json (format nil "https://app.garagiste.com/customers/~a/offerings/" user-id)
                 cookie-jar))

(defun orders (user-id cookie-jar)
  (request-json (format nil "https://app.garagiste.com/customers/~a/orders/" user-id)
                cookie-jar))

(defun shipments (user-id cookie-jar)
  (request-json (format nil "https://app.garagiste.com/customers/~a/shipments/" user-id)
                cookie-jar))


(data-lens:defalias simplify-order
    (data-lens:<>1 (lambda (it)
                     (mapcan (lambda (key el)
                               (list key el))
                             '(:unit-price :count :total-price :offering-name)
                             it))
                   (data-lens:juxt (data-lens:key "unit_price")
                                   (data-lens:key "count")
                                   (data-lens:key "total_price")
                                   (data-lens:key "offering_name"))))

(data-lens:defalias summarize-orders
    (data-lens:<>1 (data-lens:denest)
                   (data-lens:transform-head
                    (lambda (it)
                      (list :id it)))
                   (data-lens:juxt (data-lens:juxt (data-lens:<>1 'normalize-type
                                                                  (data-lens:key "type"))
                                                   (data-lens:key "id"))
                                   (data-lens:<>1 'simplify-order
                                                  (data-lens:key "attributes")))))

(defclass test-store (jsonapi-datastore recording-datastore)
  ())
