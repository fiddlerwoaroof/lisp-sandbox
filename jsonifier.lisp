(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:yason
                  :alexandria
                  :fwoar.lisputils)))

(defun group (break lis)
  (loop with result = (list)
     for (key . val) in lis
     when (equal key break) do (push () result)
     do (push (cons key val) (car result))
     finally (return result)))

(defun group-by-first (lis)
  (group (caar lis) lis))

(defun collect-pairs (reader)
  (loop for line = (funcall reader *standard-input* nil)
     while line
     for parts = (fwoar.string-utils:split #\space line :count 2)
     collect (fw.lu:vector-destructuring-bind (key val) parts
               (cons key val))))

(let* ((lines '("TITLE foo" "URL bar" "TITLE baz" "URL qwer" nil))
       (cur lines))
  (defun reader (&rest r)
    (declare (ignore r))
    (when (null cur)
      (setf cur lines))
    (prog1 (car cur)
      (setf cur (cdr cur)))))

(defun main (&optional (reader 'read-line))
  (yason:with-output (*standard-output* :indent t)
    (yason:with-array ()
      (apply 'yason:encode-array-elements
             (mapcar 'alexandria:alist-hash-table
                     (group-by-first
                      (collect-pairs reader)))))))


(defun dump ()
  (sb-ext:save-lisp-and-die "jsonifier" :toplevel 'main :executable t))
