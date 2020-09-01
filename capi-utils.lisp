(defpackage :fwoar.capi-utils
  (:use :cl))
(in-package :fwoar.capi-utils)

(defun history-pane (navigate)
  (make-instance 'capi:list-panel
                 :items '()
                 :selection-callback navigate
                 :callback-type :data
                 :external-max-width '(:character 30)))

(defun open-url (url)
  (let* ((history (history-pane (lambda (it) (format *xxx* "~&>> ~s" it))))
         (browser  (make-instance 'capi:browser-pane
                                  :url url
                                  :document-complete-callback (lambda (pane url title)
                                                                (declare (ignore pane))
                                                                (let ((new-item (make-instance 'capi:item
                                                                                               :collection history
                                                                                               :text title
                                                                                               :data url)))
                                                                  (capi:apply-in-pane-process-if-alive history
                                                                                                       'capi:append-items
                                                                                                       history (list new-item))
                                                                  (capi:apply-in-pane-process-if-alive history
                                                                                                       #'(setf capi:choice-selected-item)
                                                                                                       new-item history))
                                                                
                                                                (values)))))
    (capi:contain (make-instance 'capi:row-layout
                                 :description (list history
                                                    browser))
                  :title "Management Console"
                  :best-width 1280
                  :best-height 800)))
