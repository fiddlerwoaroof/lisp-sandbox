(defpackage :fwoar.lisp-sandbox.datalog
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.datalog)

(defun entity-gen (min-id max-id)
  (when (< max-id min-id)
    (rotatef max-id min-id))
  (lambda ()
    (+ min-id (random (- max-id min-id)))))

(defun select (options)
  (let ((options (coerce options 'vector)))
    (lambda ()
      (elt options (random (length options))))))

(defun fact-gen (entity-gen
                 attribute-gen
                 value-gen)
  (lambda ()
    (list (funcall entity-gen)
          (funcall attribute-gen)
          (funcall value-gen))))

(defun facts-gen (n
                  entity-gen
                  attribute-gen
                  value-gen)
  (let ((gen (fact-gen entity-gen attribute-gen value-gen)))
    (loop repeat n
          collect (funcall gen))))


(defun alist->triples (alist)
  (mapcar (data-lens:juxt 'sxhash
                          'car
                          'cdr)
          alist))

(defun plist->triples (plist)
  (loop for (k v) on plist by #'cddr
        collect (list (sxhash plist)
                      k
                      v)))

(defgeneric to-triples (object)
  (:documentation "convert OBJECT to a list of EAV triples")
  (:method-combination append)
  (:method append ((object hash-table))
    (serapeum:with-collector (c)
      (let ((object-id (sxhash object)))
        (maphash (lambda (k v)
                   (typecase v
                     (string (c (list object-id k v)))
                     (vector (map nil
                                  (lambda (it)
                                    (c (list object-id k it)))
                                  v))
                     (t (c (list object-id k v)))))
                 object))))
  (:method append ((object package))
    (serapeum:with-collector (c)
      (flet ((handle-symbol (s)
               (when (boundp s)
                 (c (list object :binding/variable s))
                 (alexandria:when-let ((doc (documentation s 'variable)))
                   (c (list s :documentation/variable doc))))
               (cond ((macro-function s)
                      (c (list object :binding/macro s)))
                     ((fboundp s)
                      (c (list object :binding/function s))))
               (alexandria:when-let ((doc (documentation s 'function)))
                 (c (list s :documentation/function doc)))
               (alexandria:when-let ((doc (documentation s 'setf)))
                 (c (list s :documentation/setf doc)))))
        (c (list object
                 :package/name
                 (package-name object)))
        (mapcar (lambda (nickname)
                  (c (list object
                           :package/nickname
                           nickname)))
                (package-nicknames object))
        (mapcar (lambda (use)
                  (c (list object
                           :package/uses
                           use)))
                (package-use-list object))
        (do-symbols (s object)
          (when (eql object
                     (symbol-package s))
            (c (list object
                     (case (nth-value 1
                                      (find-symbol (symbol-name s)
                                                   object))
                       (:external :symbol/external)
                       (:internal :symbol/internal)
                       (:inherited :symbol/accessible))
                     s))
            (handle-symbol s))))))
  (:method append ((object cons))
    (serapeum:with-collector (c)
      (let ((object-id (sxhash object)))
        (destructuring-bind (car . cdr) object
          (if (consp car)
              (progn (c (list object-id :car (sxhash car)))
                     (mapc #'c (to-triples car)))
              (c (list object-id :car car)))
          (if (consp cdr)
              (progn (c (list object-id :cdr (sxhash cdr)))
                     (mapc #'c (to-triples cdr)))
              (c (list object-id :cdr cdr))))))))

(defmethod to-triples append ((object plump:element))
  (serapeum:with-collector (c)
    (let ((object-id object))
      (c (list object-id :tag (plump:tag-name object))
         (list object-id :it object))
      (alexandria:when-let* ((children (plump:children object))
                             (text (and (= 1 (length children))
                                        (plump:textual-node-p (elt children 0))
                                        (elt children 0))))
        (c (list object-id :text (plump:text text))))
      (map nil (lambda (it)
                 (c (list object-id :child it)))
           (plump:child-elements object))
      (c (list object-id :next (plump:next-element object)))
      (maphash (lambda (k v)
                 (c (list object-id :attribute k))
                 (cond
                   ((equal k "class")
                    (mapcar (lambda (it)
                              (c (list object-id k it)))
                            (serapeum:split-sequence-if #'serapeum:whitespacep v)))
                   (t (c (list object-id k v)))))
               (plump:attributes object))
      (map nil (lambda (child)
                 (mapc #'c
                       (to-triples child)))
           (plump:child-elements object)))))

(defvar *database*
  '()
  "The set of triples to query")
(defvar *attribute-index*)
(defvar *attribute-cardinality*)
(defvar *entity-index*)

(defun calculate-attribute-cardinality (database)
  (fw.lu:prog1-bind (result (make-hash-table :test 'equal))
    (loop
      for triple in database
      for attribute = (attribute triple)
      do
         (incf (gethash attribute result 0)))))

(defun ea-keygen ()
  (lambda (triple)
    (list (list (entity triple)
                (attribute triple))
          (list nil
                (attribute triple))
          (list (entity triple)
                nil))))

(defun two-level-index (database key-gen)
  "Given a database and a key generator, create a hash-table with all the key-gen values"
  (loop with result = (make-hash-table :test 'equal)
        for triple in database
        do
           (loop for key in (funcall key-gen triple)
                 do
                    (push triple
                          (gethash key
                                   result)))
        finally (return result)))

(defun variablep (it)
  "Is IT a variable?"
  (and (symbolp it)
       (eql #\? (elt (symbol-name it) 0))))

(defgeneric entity (thing)
  (:documentation "get the entity for THING")
  (:method ((thing cons))
    (first thing)))

(defgeneric attribute (thing)
  (:documentation "get the attribute for THING")
  (:method ((thing cons))
    (second thing)))

(defgeneric value (thing)
  (:documentation "get the value for THING")
  (:method ((thing cons))
    (third thing)))

(defun filter-by-pattern (pattern bindings database)
  "Given BINDINGS, for each fact in DATABASE determine if PATTERN matches."
  (flet ((is-bound (variable)
           (cdr (assoc variable bindings))))
    (let* ((database (let* ((entity (entity pattern))
                            (entity-b (is-bound entity))
                            (attribute (attribute pattern))
                            (attribute-b (is-bound attribute)))
                       (cond ((and (not (variablep entity))
                                   (not (variablep attribute)))
                              (or (gethash (list entity
                                                 attribute)
                                           *attribute-index*)
                                  database))
                             ((and (not (variablep entity)))
                              (or (gethash (list entity nil)
                                           *attribute-index*)
                                  database))
                             ((and entity-b
                                   (not (variablep attribute)))
                              (or (gethash (list entity-b attribute)
                                           *attribute-index*)
                                  database))
                             ((and entity-b
                                   attribute-b)
                              (or (gethash (list entity-b attribute-b)
                                           *attribute-index*)
                                  database))
                             ((and (not (variablep attribute)))
                              (or (gethash (list nil attribute)
                                           *attribute-index*)
                                  database))
                             ((and (not (variablep entity))
                                   attribute-b)
                              (or (gethash (list entity attribute-b)
                                           *attribute-index*)
                                  database))
                             (t
                              database)))))
      (labels ((check-pattern-part (pattern target handle-binding)
                 (cond
                   ((consp pattern)
                    (destructuring-bind (var . check) pattern
                      (alexandria:if-let ((bound
                                           (serapeum:assocdr var
                                                             bindings)))
                        (when (equal bound (funcall check target))
                          t)
                        (alexandria:when-let ((val (funcall check target)))
                          (funcall handle-binding (cons var val))
                          t))))
                   ((variablep pattern)
                    (alexandria:if-let ((bound
                                         (serapeum:assocdr pattern
                                                           bindings)))
                      (when (equal bound target)
                        t)
                      (progn
                        (funcall handle-binding (cons pattern target))
                        t)))
                   (t (equal target pattern))))
               (check-pattern (triple)
                 (let ((new-bindings ()))
                   (values (every 'identity
                                  (mapcar (lambda (pat tar)
                                            (check-pattern-part pat tar
                                                                (lambda (it)
                                                                  (push it new-bindings))))
                                          pattern
                                          triple))
                           new-bindings))))
        (let ((out-bindings ()))
          (dolist (triple database out-bindings)
            (multiple-value-bind (matched new-bindings)
                (check-pattern triple)
              (when matched
                (push new-bindings
                      out-bindings)))))))))

(defun index (selector database)
  (loop with result = (make-hash-table :test 'equal)
        for triple in database
        do (push triple
                 (gethash (funcall selector triple)
                          result))
        finally (return result)))

(defun match-patterns (patterns database)
  (let* ((*attribute-index* (two-level-index database
                                             (ea-keygen)))
         (out-matches (filter-by-pattern (car patterns)
                                         ()
                                         database)))
    (dolist (pattern (cdr patterns) out-matches)
      (setf out-matches
            (mapcan (lambda (bindings)
                      (remove-duplicates
                       (mapcar (lambda (new-bindings)
                                 (append bindings new-bindings))
                               (filter-by-pattern pattern bindings database))
                       :test 'equal))
                    out-matches)))))

(defun do-q (cb out-vars patterns database)
  (let ((results (match-patterns patterns database)))
    (remove nil
            (mapcar (data-lens:∘ (data-lens:applying cb)
                                 (apply #'data-lens:juxt
                                        (mapcar #'data-lens:key
                                                out-vars)))
                    results))))


(defmacro q ((&rest out-vars) patterns &body body)
  `(do-q (lambda ,out-vars
           ,@body)
     ',out-vars
     ,patterns
     *database*))

(defun call-with-database (*database* cb)
  (funcall cb))

(defmacro with-database (database &body body)
  `(call-with-database ,database
                       (lambda ()
                         ,@body)))
