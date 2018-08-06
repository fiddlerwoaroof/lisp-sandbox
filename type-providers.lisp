(defpackage :type-providers
  (:use :cl )
  (:export ))
(in-package :type-providers)

(defun load-keys (file)
  (fset:reduce 'fset:union
               (fset:image 'fset:domain
                           (edn:parse (alexandria:read-file-into-string file)
                                      'edn:fset-lossy))))

(define-condition non-exhaustive-switch (style-warning)
  ((keys :initarg :keys :reader nes-keys)
   (cases :initarg :cases :reader nes-cases))
  (:report (lambda (c s)
             (format s "Missing cases: ~s"
                     (fset:set-difference (nes-keys c)
                                          (fset:convert 'fset:set (nes-cases c)))))))

(defmacro field-switch ((s file) &body body)
  (let* ((keys (load-keys file)))
    `(lambda (,s)
       (let (,@(fset:convert 'list
                             (fset:image (lambda (x)
                                           `(,(intern (symbol-name x)
                                                      *package*)
                                              (fset:lookup ,s ,x)))
                                         keys)))
         ,@body))))

(field-switch (s "types.edn")
  )
