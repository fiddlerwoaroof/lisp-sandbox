(defpackage :levehnstein
  (:shadowing-import-from :data-lens :pick)
  (:use :cl
        :fw.lu
        :data-lens))
(in-package :levehnstein)

  ;;;                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                      ;;;
  ;;;                                      ;;;
  ;;;                                      ;;;
  ;;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;      ;;;
  ;;;      ;;; ;;; ;;;  ;;;  ;;;  ;;;      ;;;
  ;;;      ;;;     c at h at      ;;;      ;;;
  ;;;      ;;;     co t ho t      ;;;      ;;;
  ;;;      ;;; oat coat           ;;;      ;;;
  ;;;      ;;;          ho  g dog ;;;      ;;;
  ;;;      ;;;  ;;;  ;;;; ;;; ;;; ;;;      ;;;
  ;;;      ;;;;;;;;;;;;;;;;;;;;;;;;;;      ;;;
  ;;;                                      ;;;
  ;;;                                      ;;;
  ;;;                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                      ;;;

(defun collect-by-distance (pivot items)
  (let ((items (sort (copy-seq items) '<
                     :key (op (levehnstein _ pivot)))))
    (nreverse
     (reduce (destructuring-lambda (acc (distance item))
               (if acc
                   (if (= (caar acc) distance)
                       (list* (list* (caar acc) item (cdar acc))
                              (cdr acc))
                       (list* (list distance item) acc))
                   (list (list distance item))))
             (map 'list (juxt (op (levehnstein _ pivot))
                              'identity)
                  items)
             :initial-value ()))))

(defun levehnstein-1-p (a b)
  (let-by-length (a b)
    (or (equal a b)
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

(defun build-chain (items)
  (if (null items)
      ()
      (let* ((selection (find-if (op (levehnstein-1-p _1 (car items)))
                                 (fisher-yates (cdr items))))
             (next (when selection
                     (build-chain (cons selection
                                        (without selection
                                                 (cdr items)))))))
        (if selection
            (list* (list* (car items) (car next))
                   (cdr next))
            (list* (list (car items))
                   (build-chain (cdr items)))))))

(defun get-edits (items)
  (mapcar (op (cons _1
                    (mapcar (lambda (x) (levehnstein-1 _1 x))
                            items)))
          items))

#+null
(defgeneric levehnstein-1 (a b)
  (:method ((a list) (b list))
    (if (or (null a) (listp (car a)))
        (call-next-method)
        (= (levehnstein (car (last a)) (car b))
           1)))
  (:method (a b)
    (= (levehnstein a b)
       1)))

(defun fisher-yates (seq)
  (let* ((shuffled (alexandria:copy-sequence 'vector seq))
         (n (length shuffled)))
    (loop for i from (1- n) downto 1
       for j = (random (1+ i))
       do (rotatef (elt shuffled j)
                   (elt shuffled i)))
    (alexandria:copy-sequence (type-of seq) shuffled)))

(defun levehnstein-sum (seq)
  (apply #'+
         (mapcar #'car
                 (cdr (funcall (derive 'levehnstein)
                               seq)))))

(defun pick-sequence (seq1 seq2)
  (fw.lu:if-let* ((seq1-score (levehnstein-sum seq1))
                  (seq2-score (levehnstein-sum seq2))
                  (_ (< seq1-score seq2-score)))
    (values seq1 seq1-score)
    (values seq2 seq2-score)))

(defun edit-distance-1-p (a b)
  (if (equal a b)
      t
      (if (eql (elt a 0)
               (elt b 0))
          (edit-distance-1-p (subseq a 1) (subseq b 1))
          (equal (subseq a 1) (subseq b 1)))))

(defun find-highest-out-of-place (seq)
  (multiple-value-bind (value idx)
      (funcall (alexandria:compose (maximizing #'< #'car)
                                   #'cdr
                                   (derive 'levehnstein))
               seq)
    (values (1+ idx)
            (cdr value))))

(defun swap-highest (seq)
  (let ((oop (find-highest-out-of-place seq)))
    (concatenate 'list
                 (subseq seq oop)
                 (subseq seq 0 oop))))

(defun crossover (seq)
  (if (null seq)
      seq
      (if (oddp (length seq))
          (append (crossover (cdr seq))
                  (list (car seq)))
          (let ((candidate (1+ (random (1- (length seq))))))
            (append (list (elt seq candidate))
                    (subseq seq 1 candidate)
                    (list (elt seq 0))
                    (subseq seq (1+ candidate)))))))

(defun evolve (seq steps fitness chance)
  (loop repeat steps
     for candidate = seq then (if (= 0 (random chance))
                                  (pick-sequence
                                   (and i-seq (swap-highest i-seq))
                                   (pick-sequence (crossover i-seq)
                                                  i-seq))
                                  i-seq)
     for (i-seq score) = (multiple-value-list
                          (funcall fitness seq (fisher-yates seq)))
     finally (return (values i-seq score))))

(defun evolve-halves (seq steps fitness chance)
  (let* ((halfway (random (length seq)))
         (first-part (subseq seq 0 halfway))
         (second-part (subseq seq halfway))
         (result (append (pick-sequence first-part
                                        (evolve (fisher-yates first-part) steps fitness chance))
                         (pick-sequence second-part
                                        (evolve (fisher-yates second-part) steps fitness chance)))))
    (evolve (pick-sequence seq result)
            steps fitness chance)))

(defun aggregate-items (items)
  (loop
     with candidates = items
     for (item . remainder) on items
     while remainder
     do
       (setf candidates (remove item candidates :test 'equal))
       (format t "~&~s ~s~%" item remainder)
     when (and item candidates)
     collect (loop
                for match in candidates
                when (and match (levehnstein-1 item match))
                do
                  (format t "~&~s ~s~%" item match)
                  (return (append (alexandria:ensure-list item)
                                  (alexandria:ensure-list match))))))

(defmacro let-order ((min max) (a b) &body body)
  (alexandria:once-only (a b)
    `(destructuring-bind (,min ,max) (if (<= (length ,a) (length ,b))
                                         (list ,a ,b)
                                         (list ,b ,a))
       ,@body)))

(defmacro let-by-length ((a b) &body body)
  `(let-order (,a ,b) (,a ,b)
     ,@body))

(defun levehnstein-equal (a b)
  (if (and (= (length a) (length b))
           (> (length a) 0))
      (if (eql (elt a 0) (elt b 0))
          (levehnstein-equal     (subseq a 1 nil) (subseq b 1 nil))
          (1+ (levehnstein-equal (subseq a 1 nil) (subseq b 1 nil))))
      0))

(defun window (seq len &optional acc)
  (if (= (length seq) len)
      (cons seq acc)
      (window (subseq seq 1 nil)
              len
              (cons (subseq seq 0 len) acc))))

(defun minimal-levehnstein (a b)
  (let-order (a b) (a b)
    (apply #'min
           (mapcar (op (levehnstein-equal a _))
                   (window b (length a))))))

(defun align-things (a b)
  (let-by-length (a b)
    (cond
      ((equal (length a) (length b))
       (values (coerce a 'list)
               (coerce b 'list)))
      ((equal a "")
       (values (make-sequence 'list (length b)
                              :initial-element nil)
               (coerce b 'list)))
      ((equal (elt a 0) (elt b 0))
       (multiple-value-bind (as bs) (align-things (subseq a 1) (subseq b 1))
         (values (cons (elt a 0) as)
                 (cons (elt b 0) bs))))
      (t (multiple-value-bind (as bs) (align-things a (subseq b 1))
           (values (cons nil as)
                   (cons (elt b 0) bs)))))))

(defun levehnstein (a b)
  (min (+ (minimal-levehnstein (coerce a 'list)
                               (coerce b 'list))
          (abs (- (length a)
                  (length b))))
       (multiple-value-call 'levehnstein-equal (align-things a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(every 'identity
       (list (= (levehnstein "kitten" "sitting") 3)
             (= (levehnstein "closure" "clojure") (levehnstein "clojure" "closure") 1)
             (= (levehnstein "xyx" "xyyyx") 2)
             (= (levehnstein "" "123456") 6)
             (= (levehnstein "1234" "02345") 2)
             (= (levehnstein "abcd" "ad") 2)
             (= (levehnstein "Clojure" "Clojure") (levehnstein "" "")  0)
             (= (levehnstein "ttttattttctg" "tcaaccctaccat") 10)
             (= (levehnstein "gaattctaatctc" "caaacaaaaaattt") 9)))

#+nil
(defun levehnstein (a b)
  (labels ((window-internal (seq len acc)
             (if (= (length seq) len)
                 (cons seq acc)
                 (window-internal (subseq seq 1 nil)
                                  len
                                  (cons (subseq seq 0 len) acc))))
           (min-offset-internal (haystack needle &optional (x 0) min-x min-dist)
             (if (null haystack)
                 (list min-x min-dist (length needle))
                 (let* ((search (car haystack))
                        (dist (levehnstein needle search)))
                   (destructuring-bind (min-x min-dist)
                       (if (or (null min-dist) (< dist min-dist))
                           (list x dist)
                           (list min-x min-dist))
                     (min-offset-internal (cdr haystack)
                                          needle
                                          (1+ x)
                                          min-x
                                          min-dist)))))
           (min-offset (needle haystack)
             (min-offset-internal (window-internal haystack (length needle) nil)
                                  needle))
           (levehnstein-head (len a b)
             (levehnstein (subseq a 0 len)
                          (subseq b 0 len)))
           (min-offset-tail (offset a b)
             (min-offset (subseq a offset)
                         (subseq b offset)))
           (map-reduce (comb tr init seq1 seq2)
             (reduce (lambda (acc next)
                       (funcall comb acc (apply tr next)))
                     (map 'list 'list seq1 seq2)
                     :initial-value init))
           (difference-step (char-a char-b) (if (eql char-a char-b) 0 1))
           (uneven-loop (a b &optional (x 1) (min-x x) min-total-dist)
             (destructuring-bind (tail-offset dist overlap) (min-offset-tail x a b)
               (let* ((head-distance (levehnstein-head x a b))
                      (leftover-tail (- (- (length b) x)
                                        (+ overlap tail-offset)))
                      (total-dist (+ head-distance tail-offset dist leftover-tail)))
                 (destructuring-bind (min-total-dist min-x)
                     (if (< total-dist (or min-total-dist total-dist))
                         (list total-dist x)
                         (list (or min-total-dist total-dist) min-x))
                   (if (= x (1- (length a)))
                       min-total-dist
                       (uneven-loop a b (1+ x) min-x min-total-dist)))))))
    (cond ((> (length a) (length b)) (levehnstein b a))
          ((equal a "")              (length b))
          ((equal (subseq a 1) b)    1)
          ((equal (subseq b 1) a)    1)
          ((= (length a) (length b)) (map-reduce #'+ #'difference-step 0 a b))
          (t                         (uneven-loop a b)))))

(defun without (el items)
  (remove el items :test 'equal))

(defun all-pairs (items)
  (map 'list
       (lambda (item)
         (map 'list
              (lambda (item-2)
                (list item item-2))
              (without item items)))
       items))

(defun make-word-graph (words)
  (funcall (alexandria:compose
            (op (alexandria:write-string-into-file _ "/tmp/foo.dot" :if-exists :supersede))
            (op (format nil "~&graph {~%~{~a~%~}}~%" _))
            (op (remove-duplicates _ :test 'equal))
            (over (op (format nil "~{~s~^ -- ~};" (sort (subseq _ 0 2) 'string<))))
            (op (apply 'concatenate 'list _))
            (over (alexandria:compose
                   (lambda (x) (remove-if-not (op (= _ 1)) x
                                              :key (op (car (last _)))))
                   (lambda (x) (sort x '<
                                     :key (op (car (last _)))))
                   (over (lambda (it) (concatenate 'list it (list (apply 'levehnstein it)))))))
            'all-pairs)
           words))
