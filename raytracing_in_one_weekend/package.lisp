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
  (:import-from :fwoar.lisp-sandbox.1
                #:.normal :.p :reflectance :reflect :refract
                #:random-in-unit-sphere :vec3 :random-unit-vector
                #:near-zero :dot :negate :.front-face :vec* :vec+
                #:vec- :vec/ :ray :ray-color :unit-vector :direction
                #:origin)
  (:export #:original-material
           #:lambertian-material
           #:metal-material
           #:fuzzy-metal-material
           #:dielectric-material))

(defpackage :fwoar.lisp-sandbox.1
  (:use :cl)
  (:import-from :fwoar.lisp-sandbox.material
                #:original-material
                #:lambertian-material
                #:metal-material
                #:fuzzy-metal-material
                #:dielectric-material)
  (:export #:.normal :.p :reflectance :reflect :refract
           #:random-in-unit-sphere :vec3 :random-unit-vector
           #:near-zero :dot :negate :.front-face :vec* :vec+ :vec-
           #:vec/ :ray :ray-color :unit-vector))
