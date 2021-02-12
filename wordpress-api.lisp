(defpackage :fwoar.lisp-sandbox.wordpress-api
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.wordpress-api)

(defun needs-encoding-p (char)
  (not (digit-char-p char 36)))

(defun encode-char (char)
  (format nil "%~2,'0X" (char-code char)))

(defun url-encode (s url _ __)
  (declare (ignore _ __))
  (map nil (lambda (char)
             (princ (if (needs-encoding-p char)
                        (encode-char char)
                        (string char))
                    s))
       url))

(defun uri (base path &key (per-page 10) (page 1) fields)
  (make-instance 'puri:uri
                 :scheme :https
                 :host base
                 :path path
                 :query
                 (format nil "per_page=~d&page=~d&~:[~;~:*&_fields=~{~/fwoar.lisp-sandbox.wordpress-api::url-encode/~^,~}~]"
                         per-page page fields)))

(defun json-get (uri &optional (caching t))
  (let ((drakma:*text-content-types* (acons "application" "json"
                                            drakma:*text-content-types*)))
    (let ((cache (load-time-value (vector nil nil nil))))
      #+(or)
      (when (and caching (not (elt cache 1)) (when (elt cache 0) (not (puri:uri= uri (elt cache 0))))))
      (multiple-value-bind (result _ headers) (drakma:http-request uri)
        (declare (ignore _))
        (let ((json (yason:parse result)))
          (setf (elt cache 0) uri
                (elt cache 1) json
                (elt cache 2) (pairlis '(:x-wp-total :x-wp-totalpages)
                                       (funcall (data-lens:• (data-lens:over 'parse-integer)
                                                             (apply 'data-lens:juxt
                                                                    (mapcar 'data-lens:key
                                                                            '(:x-wp-total :x-wp-totalpages))))
                                                headers)))))
      (values (elt cache 1)
              (elt cache 2)
              (elt cache 0)))))

(defparameter *title*
  (data-lens:• (data-lens.lenses:make-hash-table-lens "title")
               (data-lens.lenses:make-hash-table-lens "rendered")))

(defun view-title (it)
  (data-lens.lenses:view *title* it))

(defvar *site*)
(defun get-page-of-titles (page-number)
  (let ((uri *site*
             "/wp-json/wp/v2/posts"
             :per-page 100
             :page page-number
             :fields '("title")))
    (multiple-value-bind (r p) (json-get the-uri)
      (values (funcall (data-lens:over 'view-title)
                       r)
              p))))
