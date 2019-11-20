(defun homedir-translation (input dd)
  (declare (ignore dd))
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

(defun do-translation (input dd)
  (declare (ignore dd))
  (merge-pathnames
   (make-pathname :directory
                  (list :relative
                        "build-cache"
                        "root"
                        (format nil "~{~a~^-~}"(cdr (pathname-directory input))))
                  :defaults input)
   (merge-pathnames "asdf-corral/Contents/Resources/"
                    (user-homedir-pathname))))

(defun in-homedir ()
  (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                  :name :wild
                                  :type :wild
                                  :version :wild)
                   (user-homedir-pathname)))

(asdf:initialize-output-translations
 `(:output-translations
   :ignore-inherited-configuration
   :disable-cache
   (,(in-homedir) (:function homedir-translation))
   ("/" (:function do-translation))))

(load "~/quicklisp/setup.lisp")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trace homedir-translation do-translation))

(ql:quickload :data-lens)
