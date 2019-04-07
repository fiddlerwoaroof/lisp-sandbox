(defpackage :fwoar.raft
  (:use :cl )
  (:export ))
(in-package :fwoar.raft)

(defun take-keys (h-t &rest keys)
  (mapcar (lambda (key)
            (gethash key h-t))
          keys))

(defclass node ()
  ((%current-term :accessor term :initform 0 :initarg :current-term)
   (%voted-for    :accessor voted-for    :initform nil :initarg :leader-id)
   (%e-log        :accessor e-log
                  :initform (make-array 100 :adjustable t :fill-pointer 0))
   (%client-id    :accessor id)
   (%committed-index :accessor commit-index :initform 0)
   (%last-applied :accessor last-applied :initform 0)))


(defclass leader (node)
  ((%next-index  :accessor next-index  :initform ())
   (%match-index :accessor match-index :initform ())))

(defclass append-entry-arguments ()
  ((%term :initarg :term :reader term)
   (%leader-id :initarg :leader-id :reader leader-id)
   (%prev-log-index :initarg :prev-log-index :reader prev-log-index)
   (%prev-log-term :initarg :prev-log-term :reader prev-log-term)
   (%entries :initarg :entries :reader entries)
   (%leader-commit :initarg :leader-commit :reader leader-commit)))

(defclass entry ()
  ((%term :initarg :term :accessor term)
   (%data :initarg :data :accessor data)))

(defmethod print-object ((o entry) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "term: ~s data: ~s"
            (term o)
            (data o))))

(defun entry (term data)
  (make-instance 'entry
                 :term term
                 :data data))

(defun add-log-entry (node entry)
  (assert (= (term entry) (term node))
          (entry node)
          "term-mismatch ~s ~s"
          (term entry)
          (term node))
  (vector-push-extend entry (e-log node)))

(defun get-log-entry (follower idx)
  (elt (e-log follower)
       idx))

(defgeneric append-entries (follower args)
  (:method ((follower node) (args append-entry-arguments))
    (with-accessors ((term term)
                     (leader-id leader-id)
                     (prev-log-index prev-log-index)
                     (prev-log-term prev-log-term)
                     (entries entries)
                     (leader-commit leader-commit)) args
      
      (when (< term (term follower))
        (return-from append-entries
          (values term nil)))

      (when (/= (term (get-log-entry follower prev-log-index))
                prev-log-term)
        (return-from append-entries
          (values term nil)))

      #|If an existing entry conflicts with a new one (same index but|#
      #|   different terms), delete the existing entry and all that  |#
      #|   follow it (§5.3)                                          |#

      (loop for new-entries-index from (1+ prev-log-index)
            for (entry . rest-entries) on entries
            for existing-entry in (subseq (e-log follower) (1+ prev-log-index))
            until (/= (term entry)
                      (term existing-entry))
            finally
               (when (< new-entries-index (length (e-log follower)))
                 (setf (fill-pointer (e-log follower)) new-entries-index))

               (map nil
                    (lambda (el)
                      (vector-push-extend el (e-log follower)))
                    rest-entries))

      (when (> leader-commit (commit-index follower))
        (setf (commit-index follower)
              (min leader-commit
                   (1- (length (e-log follower))))))

      (values (term follower)
              t))))

