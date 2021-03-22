(defpackage :fwoar.lisp-sandbox.canvas-server
  (:use :cl )
  (:export
   #:send-update))
(in-package :fwoar.lisp-sandbox.canvas-server)

(defvar *ws-servers* (make-array 10 :fill-pointer 0 :adjustable t))
(defvar *stopped* ())

(defun send-update (color pos)
  (wsd:send
   (elt *ws-servers* (1- (length *ws-servers*)))
   (with-output-to-string (s)
     (yason:encode (list color
                         pos)
                   s))))

(defparameter *app*
  (lambda (env)
    (cond ((string= "/ws" (getf env :request-uri))
           (let* ((ws (wsd:make-server env))
                  (idx (if *stopped*
                           (setf (aref *ws-servers*
                                       (pop *stopped*))
                                 ws)
                           (vector-push-extend ws *ws-servers*))))
             idx
             (lambda (responder)
               responder
               (wsd:start-connection ws))))
          (t
           (format *trace-output* "~&~s~%" env)
           (list 200 '(:content-type "text/html")
                 (list
                  (spinneret:with-html-string
                    (:html
                     (:body
                      (:canvas#out :width 1000 :height 1000)
                      (:script
                       (ps:ps
                         (let* ((canvas (ps:chain document
                                                  (query-selector "canvas#out")))
                                (context (ps:chain canvas
                                                   (get-context "2d")))
                                (i-d (ps:chain context
                                               (create-image-data 1 1)))
                                (ws (ps:new (-web-socket (+ "ws://"
                                                            (ps:@ location host)
                                                            ":5000/ws")))))
                           (ps:chain ws
                                     (add-event-listener
                                      "message"
                                      (lambda (evt)
                                        (let ((data (ps:@ i-d data)))
                                          (destructuring-bind ((r g b)
                                                               (x y))
                                              (ps:chain -j-s-o-n
                                                        (parse (ps:@ evt data)))
                                            (setf (aref data 0) r
                                                  (aref data 1) g
                                                  (aref data 2) b
                                                  (aref data 3) 255)
                                            (ps:chain context
                                                      (put-image-data
                                                       i-d x y))
                                            (values))))))))))))))
           #+qwer
           (lambda )))))
(defun setup ()
  (lambda (env)
    (funcall *app* env)))
