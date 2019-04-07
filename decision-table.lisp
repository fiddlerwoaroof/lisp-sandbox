(defpackage :fwoar.decision-table
  (:use :cl )
  (:export ))
(in-package :fwoar.decision-table)

(defun array= (array1 array2 &optional (element-compare 'eql))
  (and (= (length array1)
          (length array2))
       (every element-compare array1 array2)))

(defun apply-decision-table (table inputs)
  (destructuring-bind (row col) (array-dimensions table)
    (loop with input-length = (length inputs)
          for cur-row from 0 below row
          for array-accessor = (make-array input-length
                                           :displaced-to table
                                           :displaced-index-offset (array-row-major-index table cur-row 0))
            then (adjust-array array-accessor input-length
                               :displaced-to table
                               :displaced-index-offset (array-row-major-index table cur-row 0))
          while (not (array= inputs array-accessor))
          finally (return
                    (aref table cur-row (1- col))))))

(defmacro let-order ((min max) (a b) &body body)
  (alexandria:once-only (a b)
    `(destructuring-bind (,min ,max) (if (<= (length ,a) (length ,b))
                                         (list ,a ,b)
                                         (list ,b ,a))
       ,@body)))

(defmacro let-by-length ((a b) &body body)
  `(let-order (,a ,b) (,a ,b)
     ,@body))

;; 
;;
;;
;;

(defun levehnstein-1-p (a b)
  (let-by-length (a b)
    (if (equal a b)
        t
        (cond ((= (length b)
                  (length a))
               (= (loop
                    for x from 0 below (length a)
                    when (not (eql (elt a x)
                                   (elt b x)))
                      count 1)
                  1))
              ((= (- (length b)
                     (length a))
                  1)
               (some 'identity
                     (map 'list
                          (op (equal a
                                     (remove _1 b
                                             :start _2
                                             :count 1)))
                          b (alexandria:iota (length b)))))))))
