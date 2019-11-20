(defpackage :css-norm
  (:use :cl))
(in-package :css-norm)

(defun read-block (stream)
  (when (char= (read-char stream) #\{)
    (loop
      with block = (make-string-output-stream)
      with count = 0
      for char = (read-char stream)
      until (and (char= char #\}) (= count 0))
      when (char= char #\{) do (incf count)
        when (char= char #\}) do (decf count)
          do (write-char char block)
      finally
         (return (get-output-stream-string block)))))

(defun read-to-block (stream)
  (declare (optimize (speed 3) (safety 1)))
  (with-output-to-string (s)
    (labels ((initial ()
               (let ((next-char (peek-char nil stream)))
                 (case next-char
                   (#\{ (return-from initial))
                   (#\/ (maybe-comment-start))
                   (t
                    (write-char (read-char stream)
                                s)
                    (initial)))))
             (maybe-comment-start ()
               (let ((stash (read-char stream))
                     (next-char (peek-char nil stream)))
                 (case next-char
                   (#\*
                    (read-char stream)
                    (comment)
                    (initial))
                   (t (unread-char stash
                                   stream)))))
             (comment ()
               (let ((ending nil))
                 (loop
                   (case (read-char stream)
                     (#\* (setf ending t))
                     (#\/ (when ending
                            (return-from comment)))
                     (t (when ending
                          (setf ending nil))))))))
      (initial))))

(defun partition (char string &key from-end)
  (let ((pos (position char string :from-end from-end)))
    (if pos
        (list (subseq string 0 pos)
              (subseq string (1+ pos)))
        (list nil
              string))))

(defun parse-rule (block)
  (remove-if-not #'car
                 (mapcar (serapeum:op
                           (mapcar (lambda (v1)
                                     (declare (ignorable v1))
                                     (progn
                                       (if v1
                                           (serapeum:trim-whitespace v1))))
                                   (partition #\: _)))
                         (serapeum:split-sequence #\;
                                                  (serapeum:collapse-whitespace block)))))

(defmacro one-of (chars)
  `(lambda (it)
     (case it
       (,(coerce chars 'list) t)
       (t nil))))

(defun split-to (pred str)
  (let ((split (or (= 0 (length str))
                   (position-if pred str :start 1))))
    (if (and split
             (/= 0 (length str)))
        (values (subseq str 0 split)
                (subseq str split))
        (values str
                nil))))

(defun repeatedly (fun str &optional acc)
  (declare (optimize (speed 3) (safety 1))
           (type function fun))
  (multiple-value-bind (head tail) (funcall fun str)
    (if tail
        (repeatedly fun tail
                    (cons head acc))
        (nreverse (cons head acc)))))

(defun split-selector (selector)
  (mapcan 'serapeum:tokens
          (mapcan (lambda (it)
                    (destructuring-bind (h . tt) (repeatedly (lambda (it)
                                                               (split-to (one-of "~+>")
                                                                         it))
                                                             it)
                      (cons h
                            (when tt
                              (mapcan (lambda (it)
                                        (list (subseq it 0 1)
                                              (subseq it 1)))
                                      tt)))))
                  (mapcan (lambda (it)
                            (destructuring-bind (a . r) (remove ":"
                                                                (coerce (fwoar.string-utils:split ":" it)
                                                                        'list)
                                                                :test 'equal)
                              (let ((tail (mapcar (lambda (it)
                                                    (serapeum:concat ":" it))
                                                  r)))
                                (if (equal a "")
                                    tail
                                    (cons a tail)))))
                          (repeatedly (lambda (it)
                                        (let ((first-non-ws (position-if-not 'serapeum:whitespacep
                                                                             it)))
                                          (split-to (one-of "#.[()")
                                                    (subseq it first-non-ws))))
                                      selector)))))

(defun specificity (selector)
  (macrolet ((matches-pseudo ((&rest types) v)
               (alexandria:once-only (v)
                 `(or ,@(mapcar (lambda (type)
                                  `(alexandria:starts-with-subseq ,(format nil ":~a" (string-downcase type))
                                                                  ,v))
                                types)))))
    (let ((ids 0)
          (classes 0)
          (elements 0))
      (loop for el in selector
            when (consp el)
              do
                 (case (car el)
                   (:id (incf ids))
                   (:class (incf classes))
                   (:attribute (incf classes))
                   (:element (incf elements))
                   (:pseudo (let ((v (cadr el)))
                              (cond
                                ((matches-pseudo (:link :empty :only-of-type :only-child :last-of-type :first-of-type
                                                  :last-child :first-child "nth-of-type(" "nth-last-child("
                                                  "nth-child(" :root :indeterminate :checked :disabled "enabled"
                                                  :lang :target :focus :active :hover :visited)
                                                 v)
                                 (incf classes))
                                ((or (matches-pseudo (:before :after :first-line :first-letter)
                                                     v))
                                 (incf elements))
                                (t (format t "~&unrecognized pseudoclass/element: ~s~%" v)))))))
      (list (vector ids classes elements) selector))))

(defun parse-selector (selector)
  (flet ((categorize (it)
           (case (char-downcase (elt it 0))
             (#\. (list :class it))
             (#\@ (list :media it))
             (#\[ (list :attribute it))
             (#\# (list :id it))
             (#\: (list :pseudo it))
             (#.(loop for c
                      from (char-code #\a)
                        to (char-code #\z)
                      collect (code-char c))
              (list :element it))
             (t it))))
    (let ((selector (string-trim #2=#.(format nil "~c~c" #\space #\tab)
                                 selector)))
      (if (alexandria:starts-with-subseq "@media" selector)
          (list (list #1="@media"
                      (string-trim #2#(subseq selector (length #1#)))))
          (mapcar (alexandria:compose (data-lens:over
                                       (alexandria:compose 
                                        (lambda (it)
                                          (let ((it (serapeum:trim-whitespace it)))
                                            (categorize it)))))
                                      'split-selector)
                  (split-sequence:split-sequence #\, selector))))))

(defun read-rule (stream)
  (let ((selector (funcall (alexandria:compose #'parse-selector
                                               #'serapeum:collapse-whitespace)
                           (read-to-block stream)))
        (rule (read-block stream)))
    (cons selector
          (if (equal "@media" (caar selector))
              (with-input-from-string (s rule)
                (parse-file s))
              (parse-rule rule)))))

(defun parse-file (stream)
  (loop with result = (list)
        with done = nil
        until done
        do
           (handler-case (push (read-rule stream)
                               result)
             (end-of-file (c) c (setf done t)))
        finally
           (return (nreverse result))))

(defun collapse-rule (rule)
  (let ((selector (car rule)))
    (mapcan (serapeum:op (mapcar (lambda (x) (list x _))
                                 selector))
            (cdr rule))))

(defun reconstitute (rules)
  (loop for (selector (property value)) in rules
        collect (format nil "~a { ~a: ~a; }" selector property value)))

(defun normalize-file (stream)
  (fw.lu:let-each (:be *)
    (parse-file stream)
    (mapcan #'collapse-rule *)
    (stable-sort * #'string< :key #'caadr)
    (reconstitute *)
    (serapeum:string-join * #\newline)))

(defun test-read-block ()
  (let ((strings (list "asdf cda qwer dsfa"
                       (format nil "asdf fdsaf ~% asdf qwerqw~%")
                       (format nil "{asdf fdsaf ~% asdf qwerqw~%}")
                       (format nil "asdf fdsaf {~% asdf qwerqw~%}"))))
    (loop
      for string in strings
      for n from 1
      do
         (with-input-from-string (s (format nil "{~a}" string))
           (format t "~&Test ~d: ~:[fail~;pass~]~%" n
                   (string= string (read-block s)))))))
