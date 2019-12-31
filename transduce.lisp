(defpackage :fwoar.transduce
  (:use :cl )
  (:shadow :map :filter)
  (:export #:map
           #:filter
           #:take
           #:compress
           #:build-list
           #:build-array
           #:emitting
           #:build
           #:with-transducer-arities))
(in-package :fwoar.transduce)

(declaim (inline build-list build-array emitting map filter take))

(defmacro with-transducer-arities ((accum it) &body body)
  (alexandria:with-gensyms (a-p i-p)
    `(lambda (rf)
       (declare (type (function (&optional t t) t) rf))
       (lambda (&optional (,accum nil ,a-p) (,it nil ,i-p))
         (declare (optimize (speed 3) (safety 1) (debug 0))
                  (dynamic-extent ,it))
         (cond
           (,i-p ,@body)
           (,a-p (funcall rf ,accum))
           (t (funcall rf)))))))

(defun map (mapping-fun)
  (with-transducer-arities (accum it)
    (let ((mapping-fun (alexandria:ensure-function mapping-fun)))
      (funcall rf
               accum
               (funcall mapping-fun
                        it)))))

(defun filter (filtering-fun)
  (with-transducer-arities (accum it)
    (let ((filtering-fun (alexandria:ensure-function filtering-fun)))
      (if (funcall filtering-fun it)
          (funcall rf accum it)
          accum))))

(defun take (count)
  (let ((idx 0))
    (with-transducer-arities (accum it)
      (prog1 (if (< idx count)
                 (funcall rf accum it)
                 (funcall rf accum))
        (incf idx)))))

(defun compress (&optional (test 'eql))
  (let ((v (gensym)))
    (with-transducer-arities (accum it)
      (prog1 (if (funcall test v it)
                 accum
                 (funcall rf accum it))
        (setf v it)))))

(defstruct %concat-tree
  head
  tail)
(defun %flatten-concat-tree (tree)
  (if (null (%concat-tree-head tree))
      (%concat-tree-tail tree)
      (let* ((head (%concat-tree-head tree))
             (head-tail (%concat-tree-tail head)))
        (setf (%concat-tree-tail head)
              (if head-tail
                  (cons head-tail (%concat-tree-tail tree))
                  (%concat-tree-tail tree)))
        (%flatten-concat-tree head))))

(defun build-list (rf)
  (declare (type (function (&optional t t) t) rf)
           (dynamic-extent rf))
  (lambda (&optional (a nil a-p) (i nil i-p))
    (declare (optimize (speed 3) (safety 1) (debug 0)))
    (cond
      (i-p (make-%concat-tree :head a :tail i))
      (a-p (funcall rf (%flatten-concat-tree
                        (make-%concat-tree :head a :tail nil))))
      (t (make-%concat-tree :head nil :tail nil)))))

(defun build-list-mutating (rf)
  (declare (type (function (&optional t t) t) rf)
           (dynamic-extent rf))
  (let ((l (list nil nil)))
    (declare (optimize (speed 3) (safety 1) (debug 0)))
    (lambda (&optional (a nil a-p) (i nil i-p))
      (cond
        (i-p (cdr (rplacd a (list i))))
        (a-p (funcall rf (cdr l)))
        (t l)))))

(defun build-array (rf)
  (declare (type (function (&optional t t) t) rf)
           (dynamic-extent rf))
  (let ((l (make-array 1000 :adjustable t :fill-pointer 0)))
    (lambda (&optional (a nil a-p) (i nil i-p))
      (declare (optimize (speed 3) (safety 1) (debug 0)))
      (cond
        (i-p (vector-push-extend i a)
             a)
        (a-p (funcall rf l))
        (t l)))))

(defun emitting (rf)
  (lambda (&optional (a nil a-p) (i nil i-p))
    (cond
      (i-p (format t "~&~s~%" i)
           a)
      (a-p (funcall rf :done))
      (t :going))))

(defun build (builder xf input)
  (locally (declare (optimize (debug 3)))
    (block nil
      (let ((rf (lambda (&optional (a nil a-p) (i nil i-p))
                  (declare (ignore i))
                  (cond
                    (i-p (error "this shouldn't happen"))
                    (a-p (return a))
                    (t (error "this shouldn't happen."))))))
        (etypecase input
          (list (loop with sub-fun = (funcall xf
                                              (funcall builder rf))
                      for next in input
                      for accum = (funcall sub-fun (funcall sub-fun) next)
                        then (funcall sub-fun accum next)
                      finally (funcall sub-fun accum)))
          (vector (loop with sub-fun = (funcall xf
                                                (funcall builder rf))
                        for next across input
                        for accum = (funcall sub-fun (funcall sub-fun) next)
                          then (funcall sub-fun accum next)
                        finally (funcall sub-fun accum)))
          (sequence
           (loop with sub-fun = (funcall xf
                                         (funcall builder rf))
                 with sequence = input
                 for (iterator limit from-end)
                   = (multiple-value-list (sb-sequence:make-sequence-iterator sequence))
                     then (list (sb-sequence:iterator-step sequence iterator from-end) limit from-end)
                 for accum
                   = (funcall sub-fun)
                     then (funcall sub-fun accum next)
                 until (sb-sequence:iterator-endp sequence iterator limit from-end)
                 for next = (sb-sequence:iterator-element sequence iterator)
                 finally (funcall sub-fun accum))))))))

(defun tmp ()
  (build 'build-array
         (alexandria:compose (map '1+)
                             (map (lambda (v)
                                    (* v 3)))
                             (filter 'oddp))
         #(1 2 3 4)))
