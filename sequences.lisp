;;; Extensible sequences, based on the proposal by Christophe Rhodes.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.
(cl:defpackage :sequence
  (:use)
  (:import-from :cl
                #:type-error-datum #:lambda #:&key #:&optional #:&rest
                #:&whole #:&body #:error #:null #:format #:class-of #:zerop
                #:multiple-value-bind #:cons #:cond #:and #:make-array
                #:array-element-type #:defvar #:defparameter
                #:make-list #:unless #:= #:let #:let* #:t #:make-array
                #:nil #:define-condition #:defun #:if #:eql
                #:setf #:defgeneric #:< #:rplacd #:nthcdr #:fill-pointer
                #:adjust-array #:declare #:ignore #:defmethod
                #:type-error #:apply #:array-total-size #:1- #:-
                #:array-has-fill-pointer-p #:>= #:or #:aref #:1+
                #:values #:car #:cdr #:cddr #:last #:not #:<=
                #:loop #:eq #:do* #:typecase #:gensym #:push
                #:mapcar #:defmacro #:fdefinition #:complement
                #:functionp #:flet #:list* #:funcall #:setq #:incf
                #:when #:do #:identity #:return #:floor #:ceiling #:+
                #:return-from #:> #:+ #:dotimes #:labels #:function
                #:ignorable #:prog1 #:minusp #:progn #:type #:the)
  (:export #:protocol-unimplemented #:protocol-unimplemented-operation
           #:merge #:with-sequence-iterator-functions #:make-sequence-like
           #:missing-arg #:emptyp #:length #:elt #:adjust-sequence
           #:make-sequence-iterator #:make-list-iterator #:make-vector-iterator
           #:make-simple-sequence-iterator #:iterator-step #:iterator-endp
           #:iterator-element #:iterator-index #:iterator-copy
           #:with-sequence-iterator #:find-if-not #:position #:position-if-not
           #:position-if #:subseq #:copy-seq #:fill #:nsubstitute
           #:nsubstitute-if #:nsubstitute-if-not #:substitute #:substitute-if
           #:substitute-if-not #:replace #:nreverse #:reverse #:concatenate
           #:reduce #:mismatch #:search #:delete #:delete-if #:delete-if-not
           #:remove #:remove-if #:remove-if-not #:delete-duplicates
           #:remove-duplicates #:sort #:stable-sort))
(uiop:define-package :gen-cl-user
    (:mix :sequence :cl))

(cl:in-package :sequence)

(defun missing-arg ()
  (error "A required &KEY or &OPTIONAL argument was not supplied."))


;;;; basic protocol
(define-condition protocol-unimplemented (type-error
                                          #+sbcl reference-condition)
  ((operation :initarg :operation
              :reader protocol-unimplemented-operation))
  (:default-initargs
   :operation (missing-arg)
    :references '((:sbcl :node "Extensible Sequences")))
  (:report
   (lambda (condition stream)
     (let ((operation (protocol-unimplemented-operation condition))
           (datum (type-error-datum condition)))
       (format stream "~@<The operation ~
                       ~/sb-ext:print-symbol-with-prefix/ is not ~
                       implemented for ~A which is an instance of the ~
                       ~/sb-ext:print-symbol-with-prefix/ subclass ~
                       ~S.~@:>"
               operation datum 'sequence (class-of datum)))))
  (:documentation
   "This error is signaled if a sequence operation is applied to an
   instance of a sequence class that does not support the
   operation."))

(defun protocol-unimplemented (operation sequence)
  (error 'protocol-unimplemented
         :datum sequence
         :expected-type '(or list vector)
         :operation operation))

(defgeneric emptyp (sequence)
  (:method ((s cl:list)) (null s))
  (:method ((s cl:vector)) (zerop (cl:length s)))
  (:method ((s cl:sequence)) (zerop (cl:length s)))
  (:documentation
   "Returns T if SEQUENCE is an empty sequence and NIL
   otherwise. Signals an error if SEQUENCE is not a sequence."))

(defgeneric length (sequence)
  (:method ((s cl:list)) (cl:length s))
  (:method ((s cl:vector)) (cl:length s))
  (:method ((s cl:sequence))
    (protocol-unimplemented 'length s))
  (:documentation
   "Returns the length of SEQUENCE or signals a PROTOCOL-UNIMPLEMENTED
   error if the sequence protocol is not implemented for the class of
   SEQUENCE."))

(defgeneric elt (sequence index)
  (:method ((s cl:list) index) (cl:elt s index))
  (:method ((s cl:vector) index) (cl:elt s index))
  (:method ((s cl:sequence) index)
    (cl:declare (cl:ignore index))
    (protocol-unimplemented 'elt s))
  (:documentation
   "Returns the element at position INDEX of SEQUENCE or signals a
   PROTOCOL-UNIMPLEMENTED error if the sequence protocol is not
   implemented for the class of SEQUENCE."))

(defgeneric (setf elt) (new-value sequence index)
  (:argument-precedence-order sequence new-value index)
  (:method (new-value (s cl:list) index) (setf (cl:elt s index) new-value))
  (:method (new-value (s cl:vector) index) (setf (cl:elt s index) new-value))
  (:method (new-value (s cl:sequence) index)
    (cl:declare (cl:ignore index new-value))
    (protocol-unimplemented '(setf elt) s))
  (:documentation
   "Replaces the element at position INDEX of SEQUENCE with NEW-VALUE
   and returns NEW-VALUE or signals a PROTOCOL-UNIMPLEMENTED error if
   the sequence protocol is not implemented for the class of
   SEQUENCE."))

(defgeneric make-sequence-like
    (sequence length &key initial-element initial-contents)
  (:method ((s cl:list) length
            &key (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "supplied both ~S and ~S to ~S"
                            :initial-element
                            :initial-contents
                            'make-sequence-like))
      (iep (make-list length :initial-element initial-element))
      (icp (unless (= (length initial-contents) length)
             (error "length mismatch in ~S" 'make-sequence-like))
           (let ((result (make-list length)))
             (replace result initial-contents)
             result))
      (t (make-list length))))
  (:method ((s cl:vector) length
            &key (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "supplied both ~S and ~S to ~S"
                            :initial-element
                            :initial-contents
                            'make-sequence-like))
      (iep (make-array length :element-type (array-element-type s)
                       :initial-element initial-element))
      (icp (make-array length :element-type (array-element-type s)
                       :initial-contents initial-contents))
      (t (make-array length :element-type (array-element-type s)))))
  (:method ((s cl:sequence) length &key initial-element initial-contents)
    (cl:declare (cl:ignore initial-element initial-contents length))
    (protocol-unimplemented 'make-sequence-like s))
  (:documentation
   "Returns a freshly allocated sequence of length LENGTH and of the
   same class as SEQUENCE. Elements of the new sequence are
   initialized to INITIAL-ELEMENT, if supplied, initialized to
   INITIAL-CONTENTS if supplied, or identical to the elements of
   SEQUENCE if neither is supplied. Signals a PROTOCOL-UNIMPLEMENTED
   error if the sequence protocol is not implemented for the class of
   SEQUENCE."))

(defgeneric adjust-sequence
    (sequence length &key initial-element initial-contents)
  (:method ((s cl:list) length &key initial-element (initial-contents nil icp))
    (if (eql length 0)
        nil
        (let ((olength (length s)))
          (cond
            ((eql length olength) (if icp (replace s initial-contents) s))
            ((< length olength)
             (rplacd (nthcdr (1- length) s) nil)
             (if icp (replace s initial-contents) s))
            ((null s)
             (let ((return (make-list length :initial-element initial-element)))
               (if icp (replace return initial-contents) return)))
            (t (rplacd (nthcdr (1- olength) s)
                       (make-list (- length olength)
                                  :initial-element initial-element))
               (if icp (replace s initial-contents) s))))))
  (:method ((s cl:vector) length
            &rest args &key (initial-contents nil icp) initial-element)
    (declare (ignore initial-element))
    (cond
      ((and (array-has-fill-pointer-p s)
            (>= (array-total-size s) length))
       (setf (fill-pointer s) length)
       (if icp (replace s initial-contents) s))
      ((eql (length s) length)
       (if icp (replace s initial-contents) s))
      (t (apply #'adjust-array s length args))))
  (:method ((s cl:sequence) length &rest args)
    (declare (ignore args length))
    (protocol-unimplemented 'adjust-sequence s))
  (:documentation
   "Return destructively modified SEQUENCE or a freshly allocated
   sequence of the same class as SEQUENCE of length LENGTH. Elements
   of the returned sequence are initialized to INITIAL-ELEMENT, if
   supplied, initialized to INITIAL-CONTENTS if supplied, or identical
   to the elements of SEQUENCE if neither is supplied. Signals a
   PROTOCOL-UNIMPLEMENTED error if the sequence protocol is not
   implemented for the class of SEQUENCE."))


;;;; iterator protocol

;;; The general protocol

(defgeneric make-sequence-iterator (sequence &key from-end start end)
  (:method ((s cl:vector) &key from-end (start 0) end)
    (make-vector-iterator s from-end start end))
  (:method ((s cl:list) &key from-end (start 0) end)
    (make-list-iterator s from-end start end))
  (:method ((s cl:sequence) &key from-end (start 0) end)
    (multiple-value-bind (iterator limit from-end)
        (make-simple-sequence-iterator
         s :from-end from-end :start start :end end)
      (values iterator limit from-end
              #'iterator-step #'iterator-endp
              #'iterator-element #'(setf iterator-element)
              #'iterator-index #'iterator-copy)))
  (:method ((s t) &key from-end start end)
    (declare (ignore from-end start end))
    (error 'type-error
           :datum s
           :expected-type 'sequence))
  (:documentation
   "Returns a sequence iterator for SEQUENCE or, if START and/or END
   are supplied, the subsequence bounded by START and END as nine
   values:

   1. iterator state
   2. limit
   3. from-end
   4. step function
   5. endp function
   6. element function
   7. setf element function
   8. index function
   9. copy state function

   If FROM-END is NIL, the constructed iterator visits the specified
   elements in the order in which they appear in SEQUENCE. Otherwise,
   the elements are visited in the opposite order."))

;;; magic termination value for list :from-end t
(defvar *exhausted* (cons nil nil))

(defun make-list-iterator (list from-end start end)
  (multiple-value-bind (iterator limit from-end)
      (if from-end
          (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) list)))
                 (init (if (<= (or end (cl:length list)) start)
                           termination
                           (if end
                               (last list (- (cl:length list)
                                             (1- end)))
                               (last list)))))
            (values init termination t))
          (cond
            ((not end) (values (nthcdr start list) nil nil))
            (t (let ((st (nthcdr start list)))
                 (values st (nthcdr (- end start) st) nil)))))
    (values iterator limit from-end
            (if from-end
                (lambda (list iterator from-end)
                  (declare (ignore from-end))
                  (if (eq iterator list)
                      *exhausted*
                      (do* ((cdr list (cdr cdr)))
                           ((eq (cdr cdr) iterator) cdr)))
                  (1+ iterator))
                (lambda (list iterator from-end)
                  (declare (ignore list from-end))
                  (cdr iterator)))
            (lambda (list iterator limit from-end)
              (declare (ignore list from-end))
              (eq iterator limit))
            (lambda (list iterator)
              (declare (ignore list))
              (car iterator))
            (lambda (new-value list iterator)
              (declare (ignore list))
              (setf (car iterator) new-value))
            (lambda (list iterator)
              (loop for cdr on list
                 for i from 0
                 when (eq cdr iterator)
                 return i))
            (lambda (list iterator)
              (declare (ignore list))
              iterator))))

(defun make-vector-iterator (vector from-end start end)
  (let* ((end (or end (length vector)))
         (iterator (if from-end
                       (1- end)
                       start))
         (limit (if from-end
                    (1- start)
                    end)))
    (values iterator limit from-end
            (if from-end
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (1- iterator))
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (1+ iterator)))
            (lambda (sequence iterator limit from-end)
              (declare (ignore sequence from-end))
              (= iterator limit))
            (lambda (sequence iterator)
              (aref sequence iterator))
            (lambda (new-value sequence iterator)
              (setf (aref sequence iterator) new-value))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator)
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator))))

;;; the simple protocol: the simple iterator returns three values,
;;; STATE, LIMIT and FROM-END.
(defgeneric make-simple-sequence-iterator
    (sequence &key from-end start end)
  (:method ((s cl:list) &key from-end (start 0) end)
    (if from-end
        (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) s)))
               (init (if (<= (or end (length s)) start)
                         termination
                         (if end (last s (- (length s) (1- end))) (last s)))))
          (values init termination t))
        (cond
          ((not end) (values (nthcdr start s) nil nil))
          (t (let ((st (nthcdr start s)))
               (values st (nthcdr (- end start) st) nil))))))
  (:method ((s cl:vector) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) t)
          (values start end nil))))
  (:method ((s cl:sequence) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) from-end)
          (values start end nil))))
  (:documentation
   "Returns a sequence iterator for SEQUENCE, START, END and FROM-END
   as three values:

   1. iterator state
   2. limit
   3. from-end

   The returned iterator can be used with the generic iterator
   functions ITERATOR-STEP, ITERATOR-ENDP, ITERATOR-ELEMENT, (SETF
   ITERATOR-ELEMENT), ITERATOR-INDEX and ITERATOR-COPY."))

(defgeneric iterator-step (sequence iterator from-end)
  (:method ((s cl:list) iterator from-end)
    (if from-end
        (if (eq iterator s)
            *exhausted*
            (do* ((xs s (cdr xs)))
                 ((eq (cdr xs) iterator) xs)))
        (cdr iterator)))
  (:method ((s cl:vector) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:method ((s cl:sequence) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:documentation
   "Moves ITERATOR one position forward or backward in SEQUENCE
   depending on the iteration direction encoded in FROM-END."))

(defgeneric iterator-endp (sequence iterator limit from-end)
  (:method ((s cl:list) iterator limit from-end)
    (declare (ignore from-end))
    (eq iterator limit))
  (:method ((s cl:vector) iterator limit from-end)
    (declare (ignore from-end))
    (= iterator limit))
  (:method ((s cl:sequence) iterator limit from-end)
    (declare (ignore from-end))
    (= iterator limit))
  (:documentation
   "Returns non-NIL when ITERATOR has reached LIMIT (which may
   correspond to the end of SEQUENCE) with respect to the iteration
   direction encoded in FROM-END."))

(defgeneric iterator-element (sequence iterator)
  (:method ((s cl:list) iterator)
    (car iterator))
  (:method ((s cl:vector) iterator)
    (aref s iterator))
  (:method ((s cl:sequence) iterator)
    (elt s iterator))
  (:documentation
   "Returns the element of SEQUENCE associated to the position of
   ITERATOR."))

(defgeneric (setf iterator-element) (new-value sequence iterator)
  (:method (o (s cl:list) iterator)
    (setf (car iterator) o))
  (:method (o (s cl:vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s cl:sequence) iterator)
    (setf (elt s iterator) o))
  (:documentation
   "Destructively modifies SEQUENCE by replacing the sequence element
   associated to position of ITERATOR with NEW-VALUE."))

(defgeneric iterator-index (sequence iterator)
  (:method ((s cl:list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s cl:vector) iterator) iterator)
  (:method ((s cl:sequence) iterator) iterator)
  (:documentation
   "Returns the position of ITERATOR in SEQUENCE."))

(defgeneric iterator-copy (sequence iterator)
  (:method ((s cl:list) iterator) iterator)
  (:method ((s cl:vector) iterator) iterator)
  (:method ((s cl:sequence) iterator) iterator)
  (:documentation
   "Returns a copy of ITERATOR which also traverses SEQUENCE but can
   be mutated independently of ITERATOR."))

(defun %make-sequence-iterator (sequence from-end start end)
  (typecase sequence
    (cl:vector
     (make-vector-iterator sequence from-end start end))
    (cl:list
     (make-list-iterator sequence from-end start end))
    (t
     (make-sequence-iterator sequence
                             :end end
                             :start start
                             :from-end from-end))))

(defmacro with-sequence-iterator
    ((&whole vars
             &optional iterator limit from-end-p
             step endp element set-element index copy)
               (sequence &key from-end (start 0) end) &body body)
  "Executes BODY with the elements of VARS bound to the iteration
  state returned by MAKE-SEQUENCE-ITERATOR for SEQUENCE and
  ARGS. Elements of VARS may be NIL in which case the corresponding
  value returned by MAKE-SEQUENCE-ITERATOR is ignored."
  (declare (ignore iterator limit from-end-p
                   step endp element set-element index copy))
  (let* ((ignored '())
         (vars (mapcar (lambda (x)
                         (or x (let ((name (gensym)))
                                 (push name ignored)
                                 name)))
                       vars)))
    `(multiple-value-bind (,@vars)
         (%make-sequence-iterator ,sequence ,from-end ,start ,end)
       (declare (cl:type function ,@(nthcdr 3 vars))
                (ignore ,@ignored))
       ,@body)))

(defmacro with-sequence-iterator-functions
    ((step endp elt setf index copy)
     (sequence &rest args &key from-end start end)
     &body body)
  "Executes BODY with the names STEP, ENDP, ELT, SETF, INDEX and COPY
  bound to local functions which execute the iteration state query and
  mutation functions returned by MAKE-SEQUENCE-ITERATOR for SEQUENCE
  and ARGS. STEP, ENDP, ELT, SETF, INDEX and COPY have dynamic
  extent."
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")))
    `(with-sequence-iterator
         (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
         (,sequence,@args)
       (declare (cl:ignorable ,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy))
       (flet ((,step () (setq ,nstate (funcall ,nstep ,sequence,nstate ,nfrom-end)))
              (,endp () (funcall ,nendp ,sequence,nstate ,nlimit ,nfrom-end))
              (,elt () (funcall ,nelt ,sequence,nstate))
              (,setf (new-value) (funcall ,nsetf new-value ,sequence,nstate))
              (,index () (funcall ,nindex ,sequence,nstate))
              (,copy () (funcall ,ncopy ,sequence,nstate)))
         (declare (cl:dynamic-extent #',step #',endp #',elt
                                     #',setf #',index #',copy))
         ,@body))))

(defmacro define-shadowing-generic (name (&rest args) &body options)
  (let* ((seq-position (cl:position 'sequence args))
         (pre-args (cl:subseq args 0 seq-position))
         (raw-post-args (cl:subseq args (1+ seq-position)))
         (cl-name (cl:intern (cl:symbol-name name) :cl))
         (tail (cl:member #\& raw-post-args
                          :key (lambda (x)
                                 (cl:elt (cl:symbol-name x)
                                         0))))
         (post-args (if (and tail (not (eql (car tail) '&optional)))
                        `(,@(loop for x in raw-post-args
                               for rest on raw-post-args
                               while (not (eq rest tail))
                               collect x)
                            &rest r)
                        raw-post-args)))
    (unless seq-position
      (error "no sequence argument"))
    `(defgeneric ,name ,args
       (:method (,@pre-args (sequence cl:sequence) ,@post-args)
         (declare (cl:inline))
         (apply #',cl-name ,@pre-args sequence
                ,@(cl:remove #\& post-args
                             :key (lambda (x)
                                    (cl:elt (cl:symbol-name x)
                                            0)))))
       ,@options)))

(define-shadowing-generic find-if-not
    (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))

(define-shadowing-generic position
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item))


(define-shadowing-generic position-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))


(define-shadowing-generic position-if-not
    (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))


(define-shadowing-generic subseq (sequence start &optional end))


(define-shadowing-generic copy-seq (sequence))


(define-shadowing-generic fill (sequence item &key start end))


(define-shadowing-generic nsubstitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))


(define-shadowing-generic nsubstitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))


(define-shadowing-generic nsubstitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))


(define-shadowing-generic substitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))


(define-shadowing-generic substitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))


(define-shadowing-generic substitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))


(defun %sequence-replace (sequence1 sequence2 start1 end1 start2 end2)
  (with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
      (sequence1 :start start1 :end end1)
    (declare (ignore elt1))
    (with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
        (sequence2 :start start2 :end end2)
      (do ()
          ((or (funcall endp1 sequence1 state1 limit1 from-end1)
               (funcall endp2 sequence2 state2 limit2 from-end2))
           sequence1)
        (funcall setelt1 (funcall elt2 sequence2 state2) sequence1 state1)
        (setq state1 (funcall step1 sequence1 state1 from-end1))
        (setq state2 (funcall step2 sequence2 state2 from-end2))))))

(defgeneric replace
    (sequence1 sequence2 &key start1 end1 start2 end2)
  (:argument-precedence-order sequence2 sequence1))


(define-shadowing-generic nreverse (sequence))


(define-shadowing-generic reverse (sequence))


(defgeneric concatenate (result-prototype &rest sequences)
  (:documentation
   "Implements CL:CONCATENATE for extended sequences.

    RESULT-PROTOTYPE corresponds to the RESULT-TYPE of CL:CONCATENATE
    but receives a prototype instance of an extended sequence class
    instead of a type specifier. By dispatching on RESULT-PROTOTYPE,
    methods on this generic function specify how extended sequence
    classes act when they are specified as the result type in a
    CL:CONCATENATE call. RESULT-PROTOTYPE may not be fully initialized
    and thus should only be used for dispatch and to determine its
    class."))



(define-shadowing-generic reduce
    (function sequence &key from-end start end initial-value)
  (:argument-precedence-order sequence function))


(defgeneric mismatch (sequence1 sequence2 &key from-end start1 end1
                                            start2 end2 test test-not key))


(defgeneric search (sequence1 sequence2 &key from-end start1 end1
                                          start2 end2 test test-not key))


(define-shadowing-generic delete
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item))


(define-shadowing-generic delete-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))


(define-shadowing-generic delete-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))


(define-shadowing-generic remove
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item))


(define-shadowing-generic remove-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))


(define-shadowing-generic remove-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))


(define-shadowing-generic delete-duplicates
    (sequence &key from-end test test-not start end key))


(define-shadowing-generic remove-duplicates
    (sequence &key from-end test test-not start end key))

(define-shadowing-generic sort (sequence predicate &key key))


(define-shadowing-generic stable-sort (sequence predicate &key key))


(defgeneric merge (result-prototype sequence1 sequence2 predicate &key key)
  (:documentation
   "Implements CL:MERGE for extended sequences.

    RESULT-PROTOTYPE corresponds to the RESULT-TYPE of CL:MERGE but
    receives a prototype instance of an extended sequence class
    instead of a type specifier. By dispatching on RESULT-PROTOTYPE,
    methods on this generic function specify how extended sequence
    classes act when they are specified as the result type in a
    CL:MERGE call. RESULT-PROTOTYPE may not be fully initialized and
    thus should only be used for dispatch and to determine its class.

    Another difference to CL:MERGE is that PREDICATE is a function,
    not a function designator."))

(defun %coerce-callable-to-fun (callable)
  (cl:etypecase callable
    (function callable)
    (cl:symbol (cl:symbol-function callable))))

(defmethod sequence:merge ((result-prototype cl:sequence) (sequence1 cl:sequence) (sequence2 cl:sequence)
                           (predicate cl:function) &key key)
  (let ((key-function (when key
                        (%coerce-callable-to-fun key)))
        (result (sequence:make-sequence-like
                 result-prototype (+ (length sequence1) (length sequence2))))
        endp1 elt1 key1 endp2 elt2 key2)
    (sequence:with-sequence-iterator-functions
        (step-result endp-result elt-result setelt-result index-result copy-result) (result)
      ;; TODO allow nil and fewer number of elements
      (declare (ignorable #'endp-result #'elt-result #'copy-result))
      (sequence:with-sequence-iterator-functions
          (step1 endp1 elt1 setelt1 index1 copy1) (sequence1)
        (declare (ignorable #'setelt1 #'copy1))
        (sequence:with-sequence-iterator-functions
            (step2 endp2 elt2 setelt2 index2 copy2) (sequence2)
          (declare (ignorable #'setelt2 #'copy2))
          (labels ((pop/no-key1 ()
                     (unless (setf endp1 (endp1))
                       (setf elt1 (elt1))))
                   (pop/no-key2 ()
                     (unless (setf endp2 (endp2))
                       (setf elt2 (elt2))))
                   (pop/key1 ()
                     (unless (setf endp1 (endp1))
                       (setf key1 (funcall (the function key-function)
                                           (setf elt1 (elt1))))))
                   (pop/key2 ()
                     (unless (setf endp2 (endp2))
                       (setf key2 (funcall (the function key-function)
                                           (setf elt2 (elt2))))))
                   (pop-one/no-key ()
                     (if (funcall predicate elt2 elt1) ; see comment in MERGE-LIST*
                         (prog1 elt2 (step2) (pop/no-key2))
                         (prog1 elt1 (step1) (pop/no-key1))))
                   (pop-one/key ()
                     (if (funcall predicate key2 key1)
                         (prog1 elt2 (step2) (pop/key2))
                         (prog1 elt1 (step1) (pop/key1)))))
            (declare (cl:dynamic-extent #'pop/no-key1 #'pop/no-key2
                                        #'pop/key1 #'pop/key2
                                        #'pop-one/no-key #'pop-one/key))
            ;; Populate ENDP{1,2}, ELT{1,2} and maybe KEY{1,2}.
            (cond (key-function (pop/key1) (pop/key2))
                  (t (pop/no-key1) (pop/no-key2)))
            (loop with pop-one = (if key-function #'pop-one/key #'pop-one/no-key) do
                 (cond
                   (endp2 ; batch-replace rest of SEQUENCE1 if SEQUENCE2 exhausted
                    (unless endp1
                      (replace result sequence1 :start1 (index-result) :start2 (index1)))
                    (return))
                   (endp1
                    (unless endp2
                      (replace result sequence2 :start1 (index-result) :start2 (index2)))
                    (return))
                   (t
                    (setelt-result (funcall pop-one))
                    (step-result))))))))
    result))
