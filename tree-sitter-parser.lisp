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

(defvar *current-language* :tsx)
(defun parse (src &optional (language *current-language*))
  (typecase src
    (pathname (parse (alexandria:read-file-into-string src) language))
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
                     (if (fwoar.cl-multis.interface:isa? op type)
                         (save node)
                         node))
                   node))
         (t node)))
     tree
     :traversal :postorder)))

(defun child-tagged (tag)
  (lambda (node)
    (values-list
     (remove-if-not (serapeum:op (eql tag _1))
                    (fourth (parse-thing node))
                    :key (data-lens:• 'car
                                      'parse-thing)))))


#+(or)
(defvar *empty-package* (make-package (gensym) :use ()))

#+(or)
(defvar *ht-pprint-dispatch* (copy-pprint-dispatch *print-pprint-dispatch*))

#+(or)
(set-pprint-dispatch 'hash-table
                     (lambda (s o)
                       (let ((*package* (make-package *empty-package*)))
                         (prin1 `(alexandria:alist-hash-table
                                  (list
                                   ,@(mapcar (lambda (it)
                                               `(cons ,(car it)
                                                      ,(cdr it)))
                                             (alexandria:hash-table-alist o))))
                                s)))
                     0 *ht-pprint-dispatch*)

#+(or)
(set-pprint-dispatch 'vector
                     nil
                     #+(or)
                     (lambda (s o)
                       (let ((*package* *empty-package*))
                         (typecase o
                           (string (let ((*print-pretty* nil))
                                     (prin1 o s)))
                           (t (prin1 `(vector
                                       ,@(map 'list 'identity o))
                                     s)))))
                     0 *ht-pprint-dispatch*)

(defun collect-edits (tree collector)
  (destructuring-bind (tag type source children) (parse-thing tree)
    (multiple-value-bind (_ offs) (array-displacement source)
      (declare (ignore _))
      (let ((end (+ offs (length source))))
        (flet ((tag (start-p)
                 (format nil "<~:[~;/~]span~:*~:[ class=\"~a ~{~a~^ ~}\"~;~]>"
                         start-p
                         (if tag (string-downcase tag) "")
                         (mapcar 'string-downcase
                                 (fwoar.cl-multis.interface:ancestors
                                  type)))))
          (funcall collector (fwoar.lisp-sandbox.ot-edit:insert offs (tag nil)))
          (funcall collector (fwoar.lisp-sandbox.ot-edit:insert end (tag t)))
          (mapcar (lambda (it)
                    (collect-edits it collector))
                  children))))))

#+(or)
(setf (ningle:route *a* "/2/(?<url>.*)" :regexp t)
      (lambda (_)
        (let ((*standard-output* *trace-output*))
          (fresh-line)(princ "notice: ")(prin1 _)(terpri))
        (let ((src (drakma:http-request (cadr (assoc :captures _)))))
          (flet ((outp ()
                   (spinneret:with-html
                     (:div #+(or)selector-ui
                           ;; :node-types ("expression" "identifier" "operator" "subscript-expression"
                           ;;                           "member-expression" "jsx" "pair")
                           (:pre
                            (:code :class "language-ts"
                                   (:raw
                                    (fwoar.lisp-sandbox.ot-edit:apply-edits
                                     src
                                     (serapeum:with-collector (s)
                                       (loop for next = (position #\< src)
                                               then (position #\< src :start (1+ next))
                                             while next
                                             do (s (fwoar.lisp-sandbox.ot-edit::replace-char next "&lt;")))
                                       (loop for next = (position #\> src)
                                               then (position #\> src :start (1+ next))
                                             while next
                                             do (s (fwoar.lisp-sandbox.ot-edit::replace-char next "&gt;")))
                                       (collect-edits (parse src)
                                                      #'s))))))
                           (:ul#w
                            (:li "expression"))
                           (:script
                            (:raw "const ul = document.querySelector(\"#w\");"
                                  "const dps = new Set([]);"
                                  "[].map.call(document.querySelectorAll(\".syntax-element\"),"
                                  " v=> {Array.from(v.classList).forEach(it=>dps.add(it.toLowerCase()))});"
                                  "function it(v) {const it=document.createElement(\"li\"); "
                                  "it.textContent = v; return it}"
                                  " const dpA = Array.from(dps);dpA.sort();"
                                  " dpA.forEach(cls => { if(/^[a-zA-Z-]{2,}$/.test(cls)) {ul.appendChild(it(cls))}})"))))))
            (spinneret:with-html-string
              (:style
               "input[name=expression]:checked ~ pre .expression {color: var(--zenburn-red);background-color: hsla(180,0%,0%,0.1)}"
               "input[name=jsx]:checked ~ pre .jsx {color: var(--zenburn-red);background-color: hsla(180,0%,0%,0.1)}"
               "input[name=subscript-expression]:checked ~ pre .subscript-expression {color: var(--zenburn-red);background-color: hsla(180,0%,0%,0.1)}"
               "input[name=member-expression]:checked ~ pre .member-expression {color: var(--zenburn-red);background-color: hsla(180,0%,0%,0.1)}"
               "input[name=identifier]:checked ~ pre .function .identifier {color: var(--zenburn-red)}"
               "input[name=operator]:checked ~ pre .function .operator {color: var(--zenburn-red)}"
               "input[name=pair]:checked ~ pre .pair .tag-key {color: var(--zenburn-blue)}"
               "input[name=pair]:checked ~ pre .pair .tag-value {color: var(--zenburn-green)}"
               "input[name=pair]:checked ~ pre .pair {background: var(--zenburn-fg+1)}"
               (coerce '(#\newline) 'string)
               (alexandria:read-file-into-string (truename "~/styles.css"))
               )
              (outp))))))


#+(or)
(mapcan (data-lens:•
         (lambda (it)
           (destructuring-bind (a c) it
             (mapcar (serapeum:op (list a _)) c)))
         (data-lens:juxt
          (data-lens:• (lambda (it)
                         (substitute #\- #\_ it))
                       (lambda (it)
                         (string-left-trim "_" it)))
          (data-lens:• (data-lens:over (data-lens:• (data-lens:applicable-when
                                                     (data-lens:• (lambda (it)
                                                                    (substitute #\- #\_ it))
                                                                  (lambda (it)
                                                                    (string-left-trim "_" it)))
                                                     'identity)
                                                    (data-lens:key "name")))
                       (lambda (it) (fw.lu:dive (list "rules" it "members") tsx-grammar:+data+)))))
        (gethash "supertypes" tsx))
