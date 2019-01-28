(defpackage :clos-browser
  (:use :clim-lisp :clim)
  (:export
   #:main))
(in-package :clos-browser)

(define-application-frame class-browser ()
  ((classes :initarg :classes :reader classes)
   (visible-classes :initform nil :accessor visible-classes)
   (current-class :initform nil :accessor current-class))
  (:panes (classes :application
                   :incremental-redisplay t
                   :display-function 'display-classes
                   :double-buffering t)
          (methods :application
                   :incremental-redisplay t
                   :display-function 'display-current-class
                   :scroll-bars :both)
          (int :interactor
               :scroll-bars :both))
  (:pointer-documentation t)
  (:layouts (default (vertically ()
                       (horizontally ()
                         classes methods)
                       int))
            (maximize-int (vertically ()
                            int)))
  (:default-initargs
      :classes (let ((classes ()))
                 (do-all-symbols (s (sort (remove-duplicates classes)
                                          #'string<
                                          :key #'class-name))
                   (alexandria:when-let ((class (find-class s nil)))
                     (push class classes))))))

(defun reset-application-frame ()
  (setf (visible-classes clim:*application-frame*) nil
        (current-class clim:*application-frame*) nil
        (slot-value clim:*application-frame* 'classes)
        (let ((classes ()))
          (do-all-symbols (s (sort (remove-duplicates classes)
                                   #'string<
                                   :key #'class-name))
            (alexandria:when-let ((class (find-class s nil)))
              (push class classes))))))

(define-presentation-type clos-class ())
(define-presentation-method present (object (type clos-class) stream view &key)
  (declare (ignore view))
  (format stream "#<CLOS Class: ~a>"
          (class-name object)))

(defun display-classes (frame pane)
  (updating-output (pane :unique-id (or (visible-classes frame)
                                        (classes frame))
                         :id-test 'eq)
    (loop for class in (or (visible-classes frame)
                           (classes frame))
       do (updating-output (pane :unique-id (sxhash class)
                                 :id-test 'eql
                                 :cache-value class
                                 :cache-test 'eql)
            (with-output-as-presentation (pane class 'clos-class)
              (format pane "~&~a~%" (class-name class)))))))

(defun display-current-class (frame pane)
  (updating-output (pane :unique-id (current-class frame)
                         :id-test 'eq)
    (when (current-class frame)
      (format-graph-from-roots (list (current-class frame))
                               (lambda (c stream)
                                 (present c 'clos-class :stream stream))
                               (lambda (c)
                                 (closer-mop:class-direct-superclasses c))
                               :stream pane
                               :duplicate-test 'eq
                               :graph-type :dag
                               :orientation :vertical
                               :merge-duplicates t
                               :arc-drawer (lambda (stream foo bar x1 y1 x2 y2)
                                             (updating-output (pane :unique-id (list foo bar)
                                                                    :id-test 'equal)
                                               (draw-arrow* stream x1 y1 x2 y2
                                                            :ink (make-contrasting-inks 1 0))))))))

(define-class-browser-command (com-pick-class :name t :menu t) ((the-class clos-class :gesture :select))
  (setf (current-class *application-frame*) the-class))

(define-class-browser-command (com-current-class :name t) ()
  (let ((current-class (current-class clim:*application-frame*)))
    (with-output-as-presentation (*query-io* current-class 'clos-class :single-box t)
      (format t "~&#<CLOS Class: ~s>~%" (class-name current-class)))))


(define-class-browser-command (com-refresh-classes :name t :menu t) ()
  (reset-application-frame))

(define-class-browser-command (com-filter-classes :name t :menu t) ((pattern string))
  (let ((scanner (cl-ppcre:create-scanner pattern :case-insensitive-mode t)))
    (setf (visible-classes *application-frame*)
          (remove-if-not (lambda (_)
                           (cl-ppcre:scan scanner
                                          (princ-to-string _)))
                         (classes *application-frame*)
                         :key 'class-name))))

(define-class-browser-command (com-show-hierarchy :name t) ((the-class clos-class))
  (format-graph-from-roots (list the-class)
                           (lambda (c stream)
                             (present c 'clos-class :stream stream))
                           (lambda (c)
                             (closer-mop:class-direct-superclasses c))
                           :stream *query-io*
                           :duplicate-test 'eq
                           :graph-type :tree
                           :merge-duplicates t
                           :arc-drawer (lambda (stream foo bar x1 y1 x2 y2)
                                         (declare (ignore foo bar))
                                         (draw-arrow* stream x1 y1 x2 y2
                                                      :ink (make-contrasting-inks 1 0)))))

(define-class-browser-command (com-show-subclasses :name t) ((the-class clos-class))
  (format-graph-from-roots (list the-class)
                           (lambda (c stream)
                             (present c 'clos-class :stream stream))
                           (lambda (c)
                             (closer-mop:class-direct-subclasses c))
                           :stream *query-io*
                           :duplicate-test 'eq
                           :graph-type :tree
                           :merge-duplicates t
                           :arc-drawer (lambda (stream foo bar x1 y1 x2 y2)
                                         (declare (ignore foo bar))
                                         (draw-arrow* stream x1 y1 x2 y2
                                                      :ink (make-contrasting-inks 1 0)))))

(define-class-browser-command (com-maximize-int :name t) ()
  (let ((old-view (clim:frame-current-layout clim:*application-frame*)))
    (setf (clim:frame-current-layout clim:*application-frame*)
          (case old-view
            ('default  'maximize-int)
            (t 'default)))))

(define-class-browser-command (com-exit :name "Quit"
			                                  :command-table application-commands
                                        :menu t
			                                  :provide-output-destination-keyword nil)
    ()
  (frame-exit *application-frame*))

(defvar *proc*)
(defun %main ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'class-browser)))

(defun main ()
  (setf *proc* (bt:make-thread (lambda () (%main)))))
