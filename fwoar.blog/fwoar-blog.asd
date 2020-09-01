;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :fwoar-blog
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:araneus
               #:clack
               #:ningle
               #:fwoar-lisputils)
  :serial t
  :components ((:file "package")
               (:file "data-model")
               (:file "routing")
               (:file "views")
               (:file "main")
               ))

(defsystem :fwoar-blog/test
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               )
  :serial t
  :components ((:file "t/tests")
               ))
