(defpackage :fwoar.lisp-sandbox.package
  (:use :cl )
  (:export ))

(in-package :fwoar.lisp-sandbox.package)

(defpackage :fwoar.lisp-sandbox.canvas-server
  (:use :cl )
  (:export
   #:send-update))

(defpackage :fwoar.lisp-sandbox.material
  (:use :cl)
  (:export #:original-material
           #:lambertian-material
           #:metal-material
           #:fuzzy-metal-material
           #:dielectric-material))

(defpackage :fwoar.lisp-sandbox.1
  (:use :cl)
  (:import :fwoar.lisp-sandbox.material
           #:original-material
           #:lambertian-material
           #:metal-material
           #:fuzzy-metal-material
           #:dielectric-material)
  (:export ))
