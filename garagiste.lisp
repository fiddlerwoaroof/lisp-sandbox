(defpackage :garagiste-app
  (:use :cl )
  (:export ))
(in-package :garagiste-app)

(defun get-signin-page ()
  (let ((c-j (make-instance 'drakma:cookie-jar)))
    (values (plump:parse
             (drakma:http-request "https://app.garagiste.com/users/sign_in"
                                  :cookie-jar c-j))
            c-j)))

(defun extract-auth-token (doc)
  (lquery:$1 (inline doc)
             "form input[type=hidden][name=authenticity_token]"
             (attr "value")))

(defun login-garagiste (user pass auth-token)
  )
