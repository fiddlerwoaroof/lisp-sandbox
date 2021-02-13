;; Uses death's bindings to tree-sitter: https://github.com/death/cl-tree-sitter
(defpackage :fwoar.lisp-sandbox.tree-sitter-parser
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.tree-sitter-parser)

(named-readtables:in-readtable :fare-quasiquote)

(defun tag-p (it)
  (typecase it
    (keyword t)
    (list (= 2 (length it)))))

(defun d-string (pos src)
  (let ((lines (fwoar.string-utils:get-part-modifier #\newline src)))
    lines
    (trivia:ematch pos
      (`((,s-c ,s-l) (,e-c ,e-l))
        (multiple-value-bind (_ s-l-c) (array-displacement (elt lines s-l))
          (declare (ignore _))
          (let ((start-pos (+ s-l-c s-c)))
            (multiple-value-bind (_ e-l-c) (array-displacement (elt lines e-l))
              (declare (ignore _))
              (let ((end-pos (+ e-l-c e-c)))
                (make-array (- end-pos start-pos)
                            :element-type (array-element-type src)
                            :displaced-to src
                            :displaced-index-offset start-pos)))))))))

(defun parse-thing (it)
  (trivia:ematch it
    (`((,tag ,op) ,pos ,childs)
      `(,tag ,op ,pos ,childs))
    (`(,op ,pos ,childs)
      `(nil ,op ,pos ,childs))))

(defun displace-tree (tree src)
  (serapeum:map-tree
   (lambda (node)
     (typecase node
       (cons (if (and (tag-p (car node)) (= 3 (length node)))
                 (locally (declare (optimize (debug 3)))
                   (destructuring-bind (tag op pos childs) (parse-thing node)
                     (list (if tag
                               (list tag op)
                               op)
                           (d-string pos src)
                           childs)))
                 node))
       (t node)))
   tree
   :traversal :postorder))

(defvar *current-language*)
(defun parse (src &optional (language *current-language*))
  (typecase src
    (pathname (parse language (alexandria:read-file-into-string src)))
    (string (displace-tree (cl-tree-sitter:parse-string language src)
                           src))))

(defun collect-nodes-of-type (tree type)
  (serapeum:with-collector (save)
    (serapeum:map-tree
     (lambda (node)
       (typecase node
         (cons (if (and (tag-p (car node)) (= 3 (length node)))
                   (destructuring-bind (_ op . __) (parse-thing node)
                     (declare (ignore _ __))
                     (when (eql op type)
                       (save node)))
                   node))
         (t node)))
     tree
     :traversal :postorder)))
