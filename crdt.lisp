(defpackage :fwoar.crdt
  (:use :cl)
  (:export ))
(in-package :fwoar.crdt)

(defgeneric crdt-update (op crdt))
(defgeneric crdt-query (crdt))
(defgeneric crdt-compare (less greater))
(defgeneric crdt-merge (id first second))
(defgeneric crdt-serialize (crdt))
(defgeneric crdt-deserialize (class state)
  (:method ((class symbol) state)
    (crdt-deserialize (closer-mop:class-prototype (find-class class))
                      state)))

(fw.lu:defclass+ g-counter ()
  ((lock :reader unsafe-lock
         :initform (bt:make-lock "g-counter-lock"))
   (id :reader my-id :initarg :id)
   (size :reader size :initarg :size)
   (payload :reader payload)))
(defmethod crdt-serialize ((crdt g-counter))
  (with-output-to-string (s)
    (format s "~d" (my-id crdt))
    (loop for v across (bt:with-lock-held ((unsafe-lock crdt))
                         (copy-seq (payload crdt)))
          do
             (format s "|~a" v)
          finally
             (format s "~%"))))
#+comment
(progn
  (crdt-serialize (positives *x*))
  )
(defmethod crdt-deserialize ((crdt g-counter) state)
  (let* ((parts (map 'vector 'parse-integer
                     (fwoar.string-utils:split #\|
                                               (serapeum:trim-whitespace state))))
         (result (g-counter (elt parts 0)
                            (1- (length parts)))))
    (prog1 result
      (with-slots (payload) result
        (setf payload (subseq parts 1))))))
(defmethod print-object ((o g-counter) s)
  (with-slots (id size) o
    (prin1 `(g-counter :id ,id :size ,size)
           s)))
(defmethod initialize-instance :after ((crdt g-counter) &key size)
  (with-slots (payload) crdt
    (setf payload (make-array size))))
(defmethod crdt-update ((op (eql :increment)) (crdt g-counter))
  (bt:with-lock-held ((unsafe-lock crdt))
    (incf (aref (payload crdt)
                (my-id crdt)))))
(defmethod crdt-query ((crdt g-counter))
  (reduce #'+
          (payload crdt)))
(defmethod crdt-compare ((x g-counter)
                         (y g-counter))
  (every #'<=
         (payload x)
         (payload y)))
(defmethod crdt-merge (id
                       (x g-counter)
                       (y g-counter))
  (assert (= (size x)
             (size y))
          (x y))
  (let ((z (g-counter id (size x))))
    (prog1 z
      (bt:with-lock-held ((unsafe-lock x))
        (bt:with-lock-held ((unsafe-lock y))
          (loop for i from 0 below (size x)
                do
                   (setf (aref (payload z) i)
                         (max (aref (payload x) i)
                              (aref (payload y) i)))))))))

(fw.lu:defclass+ pn-counter ()
  ((id :reader my-id :initarg :id)
   (size :reader size :initarg :size)
   (positives :reader positives)
   (negatives :reader negatives)))
(defmethod initialize-instance :after ((crdt pn-counter) &key id size)
  (with-slots (positives negatives) crdt
    (setf positives (g-counter id size)
          negatives (g-counter id size))))
(defmethod print-object ((o pn-counter) s)
  (with-slots (id size positives negatives) o
    (prin1 `(pn-counter :id ,id :size ,size)
           s)))
(defmethod payload ((object pn-counter))
  (with-slots (positives negatives) object
    (list :positives (payload positives)
          :negatives (payload negatives))))
(defmethod crdt-update ((op (eql :increment)) (crdt pn-counter))
  (crdt-update :increment (positives crdt)))
(defmethod crdt-update ((op (eql :decrement)) (crdt pn-counter))
  (crdt-update :increment (negatives crdt)))
(defmethod crdt-query ((crdt pn-counter))
  (- (crdt-query (positives crdt))
     (crdt-query (negatives crdt))))
(defmethod crdt-compare ((x pn-counter)
                         (y pn-counter))
  (and (crdt-compare (positives x) (positives y))
       (crdt-compare (negatives x) (negatives y))))
(defmethod crdt-merge (id
                       (x pn-counter)
                       (y pn-counter))
  (assert (= (size x)
             (size y))
          (x y))
  (let ((z (pn-counter id (size x))))
    (prog1 z
      (with-slots (positives negatives) z
        (with-slots ((p-x positives) (n-x negatives)) x
          (with-slots ((p-y positives) (n-y negatives)) y
            (setf positives (crdt-merge id p-x p-y)
                  negatives (crdt-merge id n-x n-y))))))))

(defun make-cluster (size class)
  (values-list
   (loop for id from 0 below size
         collect (make-instance class :id id :size size))))
