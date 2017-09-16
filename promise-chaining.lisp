(ql:quickload :parenscript)

(defpackage :parenscript-chaining
  (:use :cl :parenscript))

(in-package :parenscript-chaining)

(defclass promise ()
  ())
(defgeneric then (promise cb))

(defmacro+ps with-promise ((result-sym promise-form) &body body)
  (let ((promise-sym (gensym)))
    `(let* ((,promise-sym ,promise-form)
            ,@(loop for form in body
                    collect `(,promise-sym ((@ ,promise-sym then) (lambda (,result-sym) ,form)))))
       ,promise-sym)))


(ps
  (with-promise (val (chain window (fetch "/http/google.com")))
    (chain val (json))
    (@ val "success")))
