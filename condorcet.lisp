(defpackage :fwoar.condorcet
  (:use :cl )
  (:export ))
(in-package :fwoar.condorcet)

(defmacro define-data-class (name (&rest super-classes) &body slots)
  `(defclass ,name ,super-classes
     ,(mapcar
       (lambda (slot-name)
         `(,slot-name :initarg ,(alexandria:make-keyword slot-name) :reader ,slot-name))
       slots)))

(define-data-class ballot ()
  voter-name confirmation rankings)

(define-data-class ranking ()
  rank candidates)

(define-data-class secret-ballot ()
  confirmation rankings)

(define-data-class tally-election-request ()
  election candidates eligible-voters ballots)

(define-data-class tally-election-response ()
  election candidates voted did-not-vote rankings ballots preference-matrix strongest-path-matrix)

(define-data-class tally-election-response ()
  election candidates voted did-not-vote rankings ballots)

