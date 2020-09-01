(in-package :fwoar.blog)

(defmethod controller ((name (eql 'not-found)) _ &key key)
  (declare (ignore _))
  key)

(defun not-found (body)
  (list 404
        '()
        (ensure-list body)))

(defun found (url)
  `(302
    (:location ,url)
    ("Redirecting to: " ,url)))

#+nil
(define-view not-found (key)
  (not-found (format nil "Not found ~s" key)))

(defmethod view ((_ post-route) (post macropost))
  (spinneret:with-html-string
    (:section
     (:h* (title post))
     (:div
      (content post)))))

(defmethod view ((_ post-route) (post micropost))
  (found "/"))

(defmethod view ((_ index-route) posts)
  (spinneret:with-html-string
    (:section
     (:h* "Blog Index")
     (:div.compose
      (:form :action "/micro" :method "POST"
             (:textarea :name "content")
             (:button :type "submit")))
     (:div
      (loop for post in posts
            do (call-current-view post))))))

(defmethod view ((_ index-route) (post micropost))
  (spinneret:with-html
    (:section.post.micropost
     (content post))))

(defmethod view ((_ index-route) (post macropost))
  (spinneret:with-html
    (:section.post.macropost
     (:h* (:a :href (format nil "/~a" (slugify (title post)))
              (title post))))))
