(eval-when (:load-toplevel)
  (ql:quickload '(:macrodynamics :spinneret :alexandria)))

(defpackage #:spinneret-dynamic-tags
  (:use :cl :alexandria))
(in-package #:spinneret-dynamic-tags)

(macrodynamics:def-dynenv-var **heading-level** 1)
(macrodynamics:def-dynenv-fun make-heading (text attrs)
  (let ((heading (make-keyword (format nil "H~d" (clamp **heading-level** 1 6)))))
    `(,heading ,@attrs ,text)))

(macrodynamics:def-dynenv-macro with-heading (title (&rest attrs) &body body &environment env)
  `(spinneret:with-html
     (:section ,@attrs ,(make-heading title (list))
	       ,(macrodynamics:ct-let ((**heading-level** (1+ **heading-level**)))
		  `(progn ,@body)))))

(spinneret:deftag division (body attrs &key (title (error "A division needs a :title")))
  `(with-heading ,title (,@attrs)
     ,@body))

(spinneret:deftag checkbox (body attrs)
  `(let ((id (gensym "ID")))
     (:div.checkbox-container :id (format nil "~a-container" id)
       (:input :type "checkbox" :name id :id id)
       (:label :for id ,@attrs ,@body))))

(spinneret:deftag with-scoped-styles (((&rest styles) &rest body) attrs &key (id-sym 'id))
  `(let ((,id-sym (gensym "STYLE-SCOPE")))
     (:style
      (lass:write-sheet
       (lass:compile-sheet
	`(,(format nil "#~a" ,id-sym) ,@(loop for style in ,styles
					     collect `(quote ,style))))
       :stream spinneret:*html*))
     (:div :id ,id-sym ,@attrs
	   ,@body)))
