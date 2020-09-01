(in-package :cl-user)
(defun homedir-translation (input dd)
  (declare (ignorable dd))
  (format t "~&input ~s ~s~%" input dd)
  (merge-pathnames
   (make-pathname :directory
                  (list* :relative
                         "build-cache"
                         "homedir"
                         (cdr
                          (pathname-directory
                           (parse-namestring
                            (enough-namestring input
                                               (user-homedir-pathname))))))
                  :defaults input)
   (merge-pathnames "asdf-corral/Contents/Resources/"
                    (user-homedir-pathname))))
(defun in-homedir ()
  (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                  :name :wild
                                  :type :wild
                                  :version :wild)
                   (user-homedir-pathname)))

(defun do-translation (input dd)
  (declare (ignorable dd))
  (merge-pathnames
   (make-pathname :directory
                  (list :relative
                        "build-cache"
                        (format nil "~{~a~^-~}"(cdr (pathname-directory input))))
                  :defaults input)
   (merge-pathnames "asdf-corral/Contents/Resources/"
                    (user-homedir-pathname))))

(defun dylib-translation (input dd)
  (declare (ignorable dd))
  (format t "~&NOTICE ME: input ~s dd ~s~%" input dd)
  (merge-pathnames
   (make-pathname :directory
                  (list :relative
                        (format nil "~{~a~^-~}"(cdr (pathname-directory input))))
                  :defaults input)
   (merge-pathnames "asdf-corral/Contents/Library/"
                    (user-homedir-pathname))))

(defun is-dylib ()
  (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                  :name :wild
                                  :type "dylib"
                                  :version :wild)
                   (user-homedir-pathname)))

(asdf:initialize-output-translations
 `(:output-translations
   #+(or):ignore-inherited-configuration
   :inherit-configuration
   #+(or):disable-cache
   (,(is-dylib) (:function dylib-translation))
   #+(or)("/" (:function do-translation))))

(load "~/quicklisp/setup.lisp")

(eval-when (:execute)
  (trace dylib-translation))

#+nil
(ql:quickload :data-lens)
