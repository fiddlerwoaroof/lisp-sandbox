(ql:quickload :alexandria)
(import 'alexandria:compose)

(defun make-snoc ()
  (vector nil nil))
(defun add-to-snoc (acc a)
  (if (elt acc 1)
      (let* ((to-build (elt acc 1))
             (updated (push a (cdr to-build))))
        (setf (elt acc 1) (cdr to-build)))
      (let ((new (list a)))
        (setf (elt acc 0) new
              (elt acc 1) new)))
  acc)
(defun desnoc (acc)
  (elt acc 0))
(defun 2* (it)
  (* 2 it))
;; mapcar: conses up three lists, but keeps the steps separate
cl-user> (mapcar '1+
                 (mapcar '2*
                         (mapcar 'parse-integer
                                 '("234" "345" "567" "213"))))
(469 691 1135 427)

;; reduce: conses up one list, but at the expense of mixing all the steps together
cl-user> (desnoc
          (reduce (lambda (acc a)
                    (add-to-snoc acc
                                 (1+ (2* (parse-integer a)))))
                  '("234" "345" "567" "213")
                  :initial-value (make-snoc)))
(469 691 1135 427)

;; minor reshuffling to separate "building up the result" from the steps
cl-user> (flet ((it (rf)
                  (lambda (acc a)
                    (funcall rf
                             acc (1+ (2* (parse-integer a)))))))
           (desnoc
            (reduce (it 'add-to-snoc)
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; extract one step into its own function
cl-user> (labels ((1+-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (1+ next))))
                  (it (rf)
                    (lambda (acc next)
                      (funcall (1+-each rf) acc (2* (parse-integer next))))))
           (desnoc
            (reduce (it 'add-to-snoc)
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; continue the pattern, still only two lists are built, instead of three
cl-user> (labels ((1+-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (1+ next))))
                  (2*-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (2* next))))
                  (parse-integer-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (parse-integer next))))
                  (it (rf)
                    (lambda (acc next)
                      (funcall (parse-integer-each (2*-each (1+-each rf))) acc next))))
           (desnoc
            (reduce (it 'add-to-snoc)
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; inline IT
cl-user> (labels ((1+-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (1+ next))))
                  (2*-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (2* next))))
                  (parse-integer-each (rf)
                    (lambda (acc next)
                      (funcall rf acc (parse-integer next)))))
           (desnoc
            (reduce (parse-integer-each (2*-each (1+-each 'add-to-snoc)))
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; Abstract the "mapping" pattern
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next))))))
           (desnoc
            (reduce (funcall (mapping #'parse-integer)
                             (funcall (mapping #'2*)
                                      (funcall (mapping #'1+)
                                               'add-to-snoc)))
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; re-express as composition of steps
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next))))))
           (desnoc
            (reduce (funcall (compose (mapping #'parse-integer)
                                      (mapping #'2*)
                                      (mapping #'1+))
                             'add-to-snoc)
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; combine "add item to list" with "unwrap the result"
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (builder (acc &optional (next nil next-p))
                    (if next-p
                        (add-to-snoc acc next)
                        (desnoc acc))))
           (builder
            (reduce (funcall (compose (mapping #'parse-integer)
                                      (mapping #'2*)
                                      (mapping #'1+))
                             #'builder)
                    '("234" "345" "567" "213")
                    :initial-value (make-snoc))))
(469 691 1135 427)

;; use the builder to get the initial value
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (builder (&optional (acc nil acc-p) (next nil next-p))
                    (cond (next-p (add-to-snoc acc next))
                          (acc-p (desnoc acc))
                          (t (make-snoc)))))
           (builder
            (reduce (funcall (compose (mapping #'parse-integer)
                                      (mapping #'2*)
                                      (mapping #'1+))
                             #'builder)
                    '("234" "345" "567" "213")
                    :initial-value (builder))))
(469 691 1135 427)

;; abstract a TRANSDUCE operation: now the transformation is built up step-wise, and separated from
;;          the parts that build up the resulting data structure
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (transduce (xf build seq)
                    (funcall build
                             (reduce (funcall xf build) seq :initial-value (funcall build)))))
           (transduce (compose (mapping #'parse-integer)
                               (mapping #'2*)
                               (mapping #'1+))
                      (lambda (&optional (acc nil acc-p) (next nil next-p))
                        (cond (next-p (add-to-snoc acc next))
                              (acc-p (desnoc acc))
                              (t (make-snoc))))
                      '("234" "345" "567" "213")))
(469 691 1135 427)

;; We can trivially switch data structures now
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (transduce (xf build seq)
                    (funcall build
                             (reduce (funcall xf build) seq :initial-value (funcall build)))))
           (transduce (compose (mapping #'parse-integer)
                               (mapping #'2*)
                               (mapping #'1+))
                      (lambda (&optional (acc nil acc-p) (next nil next-p))
                        (cond (next-p (vector-push-extend next acc) acc)
                              (acc-p acc)
                              (t (make-array 0 :fill-pointer t :adjustable t))))
                      '("234" "345" "567" "213")))
#(469 691 1135 427)

(ql:quickload :data-lens)

;; We can trivially switch data structures now
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (transduce (xf build seq)
                    (funcall build
                             (reduce (funcall xf build) seq :initial-value (funcall build)))))
           (let ((result (transduce (compose (mapping (data-lens:juxt #'identity #'identity))
                                             (mapping (data-lens:over #'parse-integer))
                                             (mapping (data-lens:transform-head #'2*))
                                             (mapping (data-lens:transform-head #'1+)))
                                    (lambda (&optional (acc nil acc-p) (next nil next-p))
                                      (cond (next-p (destructuring-bind (k v) next
                                                      (setf (gethash k acc) v)) acc)
                                            (acc-p acc)
                                            (t (make-hash-table))))
                                    '("234" "345" "567" "213"))))
             (values result
                     (alexandria:hash-table-alist result))))
#<HASH-TABLE :TEST EQL :COUNT 4 {10075E2E13}>
((427 . 213) (1135 . 567) (691 . 345) (469 . 234))

;;; without EXIT-EARLY:
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (filtering (predicate)
                    (lambda (rf)
                      (lambda (acc next)
                        (if (funcall predicate next)
                            (funcall rf acc next)
                            acc))))
                  (catting ()
                    (lambda (rf)
                      (lambda (acc next)
                        (reduce rf next :initial-value acc))))
                  (exit-early (acc)
                    (throw 'done acc))
                  (taking (n)
                    (let ((taken 0))
                      (lambda (rf)
                        (lambda (acc next)
                          (format t "~&>>> ~s~%" next)
                          (if (< taken n)
                              (prog1 (funcall rf acc next)
                                (incf taken))
                              acc)))))
                  (transduce (xf build seq)
                    (funcall build
                             (catch 'done
                               (reduce (funcall xf build) seq :initial-value (funcall build))))))
           (let ((result (transduce (compose (catting)
                                             (mapping #'parse-integer)
                                             (filtering (complement #'evenp))
                                             (mapping (data-lens:juxt #'identity #'identity))
                                             (mapping (data-lens:transform-head #'2*))
                                             (mapping (data-lens:transform-head #'1+))
                                             (taking 2))
                                    (lambda (&optional (acc nil acc-p) (next nil next-p))
                                      (cond (next-p (destructuring-bind (k v) next
                                                      (setf (gethash k acc) v)) acc)
                                            (acc-p acc)
                                            (t (make-hash-table))))
                                    '(("123" "234" "345" "454") ("568" "490") ("567" "213")))))
             (values result
                     (alexandria:hash-table-alist result))))
;; >>> (247 123)
;; >>> (691 345)
;; >>> (1135 567)
;; >>> (427 213)
;; #<HASH-TABLE :TEST EQL :COUNT 2 {101585B6B3}>
;; ((691 . 345) (247 . 123))


;;; with EXIT-EARLY:
cl-user> (labels ((mapping (function)
                    (lambda (rf)
                      (lambda (acc next)
                        (funcall rf acc (funcall function next)))))
                  (filtering (predicate)
                    (lambda (rf)
                      (lambda (acc next)
                        (if (funcall predicate next)
                            (funcall rf acc next)
                            acc))))
                  (catting ()
                    (lambda (rf)
                      (lambda (acc next)
                        (reduce rf next :initial-value acc))))
                  (exit-early (acc)
                    (throw 'done acc))
                  (taking (n)
                    (let ((taken 0))
                      (lambda (rf)
                        (lambda (acc next)
                          (format t "~&>>> ~s~%" next)
                          (incf taken)
                          (if (< taken n)
                              (funcall rf acc next)
                              (exit-early (funcall rf acc next)))))))
                  (transduce (xf build seq)
                    (funcall build
                             (catch 'done
                               (reduce (funcall xf build) seq :initial-value (funcall build))))))
           (let ((result (transduce (compose (catting)
                                             (mapping #'parse-integer)
                                             (filtering (complement #'evenp))
                                             (mapping (data-lens:juxt #'identity #'identity))
                                             (mapping (data-lens:transform-head #'2*))
                                             (mapping (data-lens:transform-head #'1+))
                                             (taking 2))
                                    (lambda (&optional (acc nil acc-p) (next nil next-p))
                                      (cond (next-p (destructuring-bind (k v) next
                                                      (setf (gethash k acc) v)) acc)
                                            (acc-p acc)
                                            (t (make-hash-table))))
                                    '(("123" "234" "345" "454") ("568" "490") ("567" "213")))))
             (values result
                     (alexandria:hash-table-alist result))))
;; >>> (247 123)
;; >>> (691 345)
;; #<HASH-TABLE :TEST EQL :COUNT 2 {1015B46E23}>
;; ((691 . 345) (247 . 123))
