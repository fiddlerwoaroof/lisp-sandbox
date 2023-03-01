(defpackage :fwoar.lisp-sandbox.kruskal
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.kruskal)

(defclass disjoint-set ()
  ((%size :accessor ds-size :initform 1)
   (%parent :accessor ds-parent :initform nil)))

(defmethod initialize-instance :after ((ds disjoint-set) &key)
  (setf (ds-parent ds) ds))
(defmethod print-object ((o disjoint-set) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "size: ~d parent: ~s"
            (ds-size o)
            (unless (eql (ds-parent o) o)
              (ds-parent o)))))

(defun ds-find (ds)
  (if (eql (ds-parent ds)
           ds)
      ds
      (setf (ds-parent ds)
            (ds-find (ds-parent ds)))))


(defun ds-union (x y)
  (let ((x (ds-find x))
        (y (ds-find y)))
    (unless (eql x y)
      (when (< (ds-size x)
               (ds-size y))
        (rotatef x y))
      (setf (ds-parent y) x
            (ds-size x) (+ (ds-size x)
                           (ds-size y))))
    x))

(defun kruskal (edges)
  (let ((node-labels (make-hash-table)))
    (labels ((l (it)
               (alexandria:ensure-gethash it node-labels
                                          (make-instance 'disjoint-set)))
             (k-step (forest edges)
               (destructuring-bind ((s e . edge-rest) . rest) edges
                 (let ((ds-s (l s))
                       (ds-e (l e)))
                   (values (if (eql (ds-find ds-s)
                                    (ds-find ds-e))
                               forest
                               (progn
                                 (ds-union ds-s ds-e)
                                 (cons (list* s e edge-rest) forest)))
                           rest)))))
      (loop for (s e) in edges do
        (l s)
        (l e))
      (loop for (forest %edges) = (multiple-value-list (k-step () edges))
              then (multiple-value-list (k-step forest %edges))
            while %edges
            finally (return (reverse forest))))))

#|
|---+---+---+---|
| a | b | c | d |
|---+---+---+---|
| e | f | g | h |
|---+---+---+---|
| j | k | l | m |
|---+---+---+---|
| n | o | p | q |
|---+---+---+---|

|---+---+---+---|
| a | b   c   d |
|   +   +---+   |
| e   f   g | h |
|---+---+   +---|
| i   j   k   l |
|   +---+---+   |
| m | n   o   q |
|---+---+---+---|
|#
(defun grid-graph (max)
  (flet ((w ()
           (random max)))
    (stable-sort `((a b ,(w)) (a e ,(w)) ;; (a f ,(w))
                   (b c ,(w)) (b f ,(w)) ;; (b g ,(w))
                   (c d ,(w)) (c g ,(w)) ;; (c h ,(w))
                   #|      |# (d h ,(w)) ;;
                   (e f ,(w)) (e j ,(w)) ;; (e k ,(w))
                   (f g ,(w)) (f k ,(w)) ;; (f l ,(w))
                   (g h ,(w)) (g l ,(w)) ;; (g m ,(w))
                   #|      |# (h m ,(w))
                   (j k ,(w)) (j n ,(w)) ;; (j o ,(w))
                   (k l ,(w)) (k o ,(w)) ;; (k p ,(w))
                   (l m ,(w)) (l p ,(w)) ;; (l q ,(w))
                   #|      |# (m q ,(w))
                   (n o ,(w))
                   (o p ,(w))
                   (p q ,(w)))
                 '<
                 :key #'third)))

(defun grid-edges (w h)
  (remove-if-not (lambda (it)
                   (destructuring-bind ((s-x s-y)
                                        (e-x e-y))
                       it
                     (and (< s-x w)
                          (< e-x w)
                          (< s-y h)
                          (< e-y h))))
                 (loop for x below w
                       append (loop for y below h
                                    append (list (list (list x y)
                                                       (list x (1+ y)))
                                                 (list (list x y)
                                                       (list (1+ x) y)))))))

(defun print-grid (grid edge-map)
  (let ((edges (make-hash-table :test #'equal)))
    (loop
      for (s e) in edge-map do
        (push e (gethash s edges)))
    (princ #\+)
    (loop repeat (1- (* (array-dimension grid 1) 4)) do
      (princ "-"))
    (princ #\+)
    (terpri)
    (loop
      for x below (array-dimension grid 0) do
        (princ #\|)
        (loop for y below (array-dimension grid 1)
              do
                 (format t " ~a " (aref grid x y))
              when (< y (1- (array-dimension grid 1)))
                do (princ (if (member (list x (1+ y))
                                      (gethash (list x y)
                                               edges)
                                      :test #'equal)
                              #\space
                              #\|)))
        (princ #\|)
        (terpri)
      when (< x (1- (array-dimension grid 0))) do
        (princ #\|)
        (loop for y below (array-dimension grid 1)
              do
                 (princ
                  (if (member (list (1+ x) y)
                              (gethash (list x y)
                                       edges)
                              :test #'equal)
                      "   "
                      "---"))
              when (< y (1- (array-dimension grid 1)))
                do (princ #\+))
        (princ #\|)
        (terpri))
    (princ #\+)
    (loop repeat (1- (* (array-dimension grid 1) 4)) do
      (princ "-"))
    (princ #\+)
    (terpri)))

(defun fully-connected (n max-weight)
  (flet ((symbolicate (it)
           (coords->symbol (list (floor it (1+ (floor (sqrt n))))
                                 (mod it (1+ (floor (sqrt n)))))
                           n)))
    (let ((nodes (loop for x below n
                       collect x)))
      (loop
        for (h . tail) on nodes
        append (loop for it in tail
                     collect (list (symbolicate h)
                                   (symbolicate it)
                                   (random max-weight)))))))

(defun symbol->coords (sym side-len)
  (let* ((v (- (char-code (elt (symbol-name sym) 0))
               #.(char-code #\A)))
         (v (if (> v 8)
                (1- v)
                v)))
    (list (floor v side-len)
          (mod v side-len))))

(defun coords->symbol (coords side-len)
  (let ((it (+ (* side-len (elt coords 0))
               (elt coords 1))))
    (intern
     (string
      (code-char
       (+ #.(char-code #\A)
          (if (>= it 8)
              (1+ it)
              it)))))))

(defun ->graph (edges &optional (s t))
  (format s "graph {~%~{~:@{~2t~a -- ~a [label=\"~a\"]~%~}~}~&}~%"
          edges))
