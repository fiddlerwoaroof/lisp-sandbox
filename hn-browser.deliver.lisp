(defun utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

(setq system:*file-encoding-detection-algorithm*
      (substitute 'utf-8-file-encoding
                  'system:locale-file-encoding
                  system:*file-encoding-detection-algorithm*))
(set-default-character-element-type 'simple-char)

(in-package :cl-user)

(format t "~&CURDIR: ~a~%" (truename "."))
(load-all-patches)
(load "~/quicklisp/setup.lisp")
(ql:quickload '(:cells :data-lens :drakma :lquery :fwoar-lisputils :alexandria :serapeum))
(compile-file "/Users/edwlan/git_repos/git.fiddlerwoaroof.com/lisp-sandbox/hn-browser.lisp")
(load "/Users/edwlan/git_repos/git.fiddlerwoaroof.com/lisp-sandbox/hn-browser")

(deliver (intern "STARTUP" "FWOAR.LISP-SANDBOX.HN-BROWSER")
         (create-macos-application-bundle
          "HNReader.app"
          :document-types nil
          :identifier "fwoar.cj.HNReader"
          :version "0.0.never")
         0
         :split :resources
         :interface :capi
         ;;:startup-bitmap-file nil
         )
