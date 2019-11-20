(progn
  (defparameter *loads* ())
  (defparameter *load-depth* 0)
  (ccl:advise asdf:operate
              (let ((current-load (list)))
                (incf *load-depth*)
                (push (cons *load-depth* arglist)
                      *loads*)
                (unwind-protect (:do-it)
                  (decf *load-depth*)))
              :when :around))
