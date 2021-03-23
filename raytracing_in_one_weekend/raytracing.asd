;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :raytracing
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:clack
               #:fwoar-lisputils
               #:local-time
               #:parenscript
               #:spinneret
               #:uiop
               #:websocket-driver
               #:yason
               (:require :sb-concurrency))
  :serial t
  :components ((:file "package")
               (:file "canvas-server")
               (:file "material")
               (:file "vector-utils")
               (:file "1")
               (:file "scenes")))
