(defpackage :fwoar.graphql
  (:import-from :data-lens :key :over :juxt)
  (:import-from :serapeum :defalias)
  (:use :cl )
  (:export ))
(in-package :fwoar.graphql)

#|
;; query {
;;   search(query: "topic:common-lisp", type: REPOSITORY, first: 100) {
;;     edges {
;;       node {
;;         ... on Repository {
;;           owner {
;;             login
;;           }
;;           name
;;           description 
;;           pushedAt
;;         }
;;       }
;;     }
;;   }
;; }
|#

(defun camelize (name)
  (fw.lu:with (parts (fwoar.string-utils:split #\- (string-downcase name)))
    (format nil "~a~{~:(~a~)~}"
            (elt parts 0)
            (map 'list #'string-capitalize (subseq parts 1)))))

(defun handle-term (term)
  (typecase term
    (symbol (camelize term))
    (t term)))

(defun sexp->query (s expr)
  (destructuring-bind (op &rest body) expr
    (pprint-logical-block (s nil)
      (pprint-logical-block (s nil)
        (pprint-indent :block 4 s)
        (if (listp op)
            (format s "~a(~{~a~^, ~})"
                    (handle-term (car op))
                    (loop for (key value) on (cdr op) by #'cddr
                          collect (format nil "~a:~a" (handle-term key) value)))
            (princ (handle-term op) s))
        (princ " {" s)
        (loop for expr in body
              do
                 (pprint-newline :mandatory s)
              if (listp expr) do
                (sexp->query s expr)
              else do
                (princ (handle-term expr)
                       s)))
      (pprint-newline :mandatory s)
      (format s "}"))))

(defun dive (&rest keys)
  (lambda (map)
    (declare (dynamic-extent map))
    (fw.lu:dive keys map)))

(defalias <>
  'alexandria:compose)

(defun %jq (&rest query)
  (lambda (map)
    (reduce (lambda (acc next)
              (typecase next
                (list (ecase (car next)
                        (:juxt (funcall (apply 'juxt
                                               (mapcar '%jq
                                                       (cdr next)))
                                        acc))
                        (:dive (funcall (apply '%jq (cdr next))
                                        acc))
                        (:map (funcall (over (apply '%jq (cdr next)))
                                       acc))))
                (t (gethash next acc))))
            query
            :initial-value map)))

(defmacro jq (&rest query)
  `(%jq ,@(mapcar (lambda (v) `',v)
                  query)))

(defun format-results (results)
  (format t "~:{* ~a/~a ~a~%~2t~a~%~%~}"
          results))


;; .data.search.edges[]
;; | .node
;; | "* "+ .owner.login + "/" + .name + " " + .pushedAt + "\n  " + .description + "\n"'

(defalias extract-data
  (jq "data"
      "search"
      "edges"
      (:map "node"
            (:juxt (:dive "owner"
                          "login")
                   "name"
                   "pushedAt"
                   "description"))))

(defun search-for-topic-repos (topic)
  `(query ((search query ,(format nil "\"topic:~a\"" topic)
                   type "REPOSITORY"
                   first "100")
           (edges
            (node
             ("... on Repository"
              (owner login)
              name
              description
              pushed-at))))))

(defun github-graphql (token query)
  (let ((drakma:*text-content-types* (acons "application" "json"
                                            drakma:*text-content-types*)))
    (yason:parse (drakma:http-request "https://api.github.com/graphql"
                                      :method :post 
                                      :content   (yason:with-output-to-string* ()
                                                   (yason:with-object ()
                                                     (yason:encode-object-element
                                                      "query"
                                                      (with-output-to-string (s)
                                                        (sexp->query s query)))))
                                      :additional-headers `(("Authorization" . ,(format nil "bearer ~a" token)))))))
