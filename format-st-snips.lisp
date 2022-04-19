(defpackage :fwoar.lisp-sandbox.format-st-snips
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.format-st-snips)

(defparameter *counters-css*
  "@counter-style objectio {
  system: extends numeric;
  prefix: \"arg.\";
}

@counter-style sedcontra {
  system: extends numeric;
  prefix: \"s.c. \";
}

@counter-style responsio {
  system: extends numeric;
  prefix: \"ad \";
}
")

(defparameter *fonts-css*
  "@font-face {
font-family: century_supra_a;
font-style: normal;
font-weight: normal;
font-stretch: normal;
font-display: auto;
src: url('./fonts/century_supra_a_regular.woff2') format('woff2');
}

@font-face {
font-family: century_supra_a;
font-style: italic;
font-weight: normal;
font-stretch: normal;
font-display: auto;
src: url('./fonts/century_supra_a_italic.woff2') format('woff2');
}

@font-face {
font-family: century_supra_a;
font-style: normal;
font-weight: bold;
font-stretch: normal;
font-display: auto;
src: url('./fonts/century_supra_a_bold.woff2') format('woff2');
}

@font-face {
font-family: century_supra_a;
font-style: italic;
font-weight: bold;
font-stretch: normal;
font-display: auto;
src: url('./fonts/century_supra_a_bold_italic.woff2') format('woff2');
}

@font-face {
font-family: concourse_4;
font-style: normal;
font-weight: normal;
font-stretch: normal;
font-display: auto;
src: url('./fonts/concourse_4_regular.woff2') format('woff2');
}

@font-face {
font-family: concourse_4;
font-style: italic;
font-weight: normal;
font-stretch: normal;
font-display: auto;
src: url('./fonts/concourse_4_italic.woff2') format('woff2');
}

@font-face {
font-family: concourse_4;
font-style: normal;
font-weight: bold;
font-stretch: normal;
font-display: auto;
src: url('./fonts/concourse_4_bold.woff2') format('woff2');
}

@font-face {
font-family: concourse_4;
font-style: italic;
font-weight: bold;
font-stretch: normal;
font-display: auto;
src: url('./fonts/concourse_4_bold_italic.woff2') format('woff2');
}
")

(defun ht->vector (h-t)
  (let* ((keys (alexandria:hash-table-keys h-t))
         (base (reduce #'min keys :initial-value 100000))
         (limit (reduce #'max keys :initial-value 0))
         (result (make-array (1+ (- limit base))
                             :initial-element nil)))
    (prog1 result
      (mapc (lambda (key)
              (setf (aref result (- key base))
                    (gethash key h-t)))
            keys))))

(defun path-hash-table (h-t path value)
  (let ((most-specific (car (last path))))
    (prog1 h-t
      (loop with cur = h-t
            for it in (butlast path)
            do
               (setf cur
                     (alexandria:ensure-gethash it cur
                                                (make-hash-table :test (hash-table-test h-t))))
            finally
               (setf (gethash most-specific cur) value)))))

(defun cycle (&rest vs)
  (let ((cur vs))
    (lambda ()
      (prog1 (car cur)
        (if (cdr cur)
            (setf cur (cdr cur))
            (setf cur vs))))))

(defun alternate (l1 l2)
  (remove-if 'null
             (mapcan 'identity
                     (funcall (data-lens:zipping 'list :fill-value nil)
                              l1 l2))))

(defun collate-it (forms)
  (let ((h-t (make-hash-table :test 'equal)))
    (prog1 h-t
      (mapc (lambda (it)
              (path-hash-table h-t
                               (cons (second it)
                                     (mapcan 'decode-ref
                                             (parse-ref (third it))))
                               (list (cons (string (second it)) (parse-ref-o (third it)))
                                     (cleanup (fourth it)))))
            forms))))

(defmacro regex-cond (it &body body)
  it body (error "macrolet"))
(defun cleanup (it)
  (macrolet ((regex-cond-case (target regex (beg end) &body body)
               `(multiple-value-bind (,beg ,end) (cl-ppcre:scan ,regex ,target)
                  (declare (ignorable ,beg ,end))
                  (when ,beg
                    (return
                      (progn ,@body)))))
             (regex-cond (it &body cases)
               (alexandria:once-only (it)
                 `(block nil
                    ,@(loop for case in cases
                            if (equal t (car case))
                              collect `(progn ,@(cdr case))
                            else collect `(regex-cond-case ,it ,@case))))))
    (with-simple-restart (skip "skip ~s" it)
      (serapeum:trim-whitespace
       (regex-cond it
         ("arg[.] [0-9]+" (b e)
                          (subseq it e))
         ("s[.] c[.] [0-9]+" (b e)
                             (subseq it e))
         ("s[.] c[.]" (b e)
                      (subseq it e))
         ("co[.]" (b e)
                  (subseq it e))
         ("pr[.]" (b e)
                  (subseq it e))
         ("ad [0-9]+" (b e)
                      (subseq it e))
         ("ad arg[.]" (b e)
                      (subseq it e))
         (t (break)))))))

(defun parse-ref-o (ref)
  (let* ((book (funcall (data-lens:regex-match "^[I-]+") ref))
         (ref (subseq ref (length book))))
    (multiple-value-bind (a b)
        (serapeum:with-collectors (w n)
          (let ((cleaner (cycle #'w (data-lens:• #'n #'parse-integer))))
            (mapcar (lambda (it)
                      (funcall (funcall cleaner)
                               (apply 'subseq ref it)))
                    (loop for x from 0 below (length ref)
                          for idx-f = (if (evenp x)
                                          'position-if
                                          'position-if-not)
                          for start-idx = 0 then idx
                          for idx = (funcall idx-f 'digit-char-p ref :start start-idx)
                          collect (list start-idx idx)
                          while idx))))
      (list* book
             (funcall (data-lens:• (data-lens:over
                                    (data-lens:applying
                                     (lambda (i j)
                                       (if (= j 0)
                                           (format nil "~a" i)
                                           (format nil "~a ~a" i j)))))
                                   (data-lens:zipping 'list :fill-value 0))
                      a b)))))

(defun parse-ref (ref)
  (if (equal ref "pr.")
      (list 1000)
      (let* ((book (funcall (data-lens:regex-match "^[I-]+") ref))
             (ref (subseq ref (length book))))
        (multiple-value-bind (a b)
            (serapeum:with-collectors (w n)
              (let ((cleaner (cycle #'w (data-lens:• #'n #'parse-integer))))
                (mapcar (lambda (it)
                          (funcall (funcall cleaner)
                                   (apply 'subseq ref it)))
                        (loop for x from 0 below (length ref)
                              for idx-f = (if (evenp x)
                                              'position-if
                                              'position-if-not)
                              for start-idx = 0 then idx
                              for idx = (funcall idx-f 'digit-char-p ref :start start-idx)
                              collect (list start-idx idx)
                              while idx))))
          (let ((a (mapcar (lambda (it)
                             (string-case:string-case (it)
                               ("pr." 1)
                               ("q." 2)
                               ("a." 3)
                               ("arg." 4)
                               ("s.c." 5)
                               ("co." 6)
                               ("ad" 7)
                               ("adarg." 8)))
                           a)))
            (list* (string-case:string-case (book)
                     ("I" 1)
                     ("I-II" 2)
                     ("II-II" 3)
                     ("III" 4))
                   (funcall (data-lens:• (data-lens:over
                                          (data-lens:applying
                                           (lambda (i j)
                                             (+ (* i 1000)
                                                j))))
                                         (data-lens:zipping 'list :fill-value 0))
                            a b)))))))

(defun format-text (forms)
  (let ((old-s-o *standard-output*)
        (dir (ensure-directories-exist "/Users/edwlan/summa-html/"))
        (files '("prooemium.html")))
    (unwind-protect
         (progn (setf *standard-output* (open (merge-pathnames (car files)
                                                               dir)
                                              :direction :output
                                              :if-exists :supersede))
                (spinneret:with-html
                  (:doctype)
                  (:html
                   (:meta :charset "utf-8")
                   (:link :rel "stylesheet"
                          :href "../style.css")
                   (:body
                    (:main
                     (let ((counter 0)
                           (question-counter -1)
                           (state))
                       (:nav
                        (:a :href "index.html" "index") "–" (:a :href "q-1.html" "next"))
                       (funcall (data-lens:over
                                 (lambda (it)
                                   (let ((id (format nil "~a~a"
                                                     (second it)
                                                     (remove #\. (third it)))))
                                     (cond
                                       ((cl-ppcre:scan "arg[.]" (fourth it))
                                        (case state
                                          ((:sc :ad)
                                           (princ "</ol>" spinneret:*html*))
                                          (:corp
                                           (princ "</section>" spinneret:*html*)))
                                        (when (cl-ppcre:scan "^.*arg[.].*Ad.*sic proceditur" (fourth it))
                                          (when (> counter 0)
                                            (princ "</div>" spinneret:*html*))
                                          (:h2 (format nil "Articulus ~@R"
                                                       (incf counter)))
                                          (princ "<div>" spinneret:*html*))
                                        (unless (eql state :arg)
                                          (princ "<ol class=\"arg\">" spinneret:*html*)
                                          (setf state :arg))
                                        (:li.obj :id id
                                                 (elt (fwoar.string-utils:split " " (fourth it) :count 8) 8)))
                                       ((cl-ppcre:scan "s[.] c[.]" (fourth it))
                                        (case state
                                          ((:arg :ad)
                                           (princ "</ol>" spinneret:*html*))
                                          (:corp
                                           (princ "</section>" spinneret:*html*)))
                                        (unless (eql state :sc)
                                          (princ "<ol class=\"sc\">" spinneret:*html*)
                                          (setf state :sc))
                                        (:li.sc :id id
                                                (elt (fwoar.string-utils:split " " (fourth it) :count 8) 8)))
                                       ((cl-ppcre:scan "co[.]" (fourth it))
                                        (case state
                                          ((:arg :sc :ad)
                                           (princ "</ol>" spinneret:*html*)))
                                        (unless (eql state :corp)
                                          (princ "<section class=\"resp\">" spinneret:*html*)
                                          (setf state :corp))
                                        (:div.co :id id
                                                 (elt (fwoar.string-utils:split " " (fourth it) :count 7) 7)))
                                       ((cl-ppcre:scan "pr[.]" (fourth it))
                                        (if (>= question-counter 0)
                                            (let ((new-file (format nil "q-~d.html"
                                                                    (incf question-counter))))
                                              (push (list new-file
                                                          (format nil "~a ~a"
                                                                  (second it)
                                                                  (third it)))
                                                    files)
                                              (close *standard-output*)
                                              (setf *standard-output*
                                                    (open (merge-pathnames new-file dir)
                                                          :direction :output
                                                          :if-exists :supersede))
                                              (:doctype html)
                                              (:link :rel "stylesheet"
                                                     :href "../style.css")
                                              (:meta :charset "utf-8")
                                              (princ "<main>")
                                              (:nav
                                               (if (> question-counter 0)
                                                   (progn
                                                     (:a :href (format nil "./~a"
                                                                       (if (= 1 question-counter)
                                                                           "prooemium.html"
                                                                           (format nil "q-~d.html"
                                                                                   (1- question-counter))))
                                                         "prev"))
                                                   (progn (:a :href "index.html" "index")))
                                               " – "
                                               (format nil "~a ~a" (second it) (subseq (third it)
                                                                                       0
                                                                                       (- (length
                                                                                           (third it))
                                                                                          3)))
                                               " – "
                                               (:a :href (format nil "./~a"
                                                                 (if (= 0 question-counter)
                                                                     "index.html"
                                                                     (format nil "q-~d.html"
                                                                             (1+ question-counter))))
                                                   (if (= 0 question-counter)
                                                       "index"
                                                       "next")))
                                              (setf counter 0)
                                              (:h1 (format nil "~a ~a"
                                                           (second it)
                                                           (subseq (third it)
                                                                   0
                                                                   (- (length (third it))
                                                                      3)))))
                                            (prog1 (:h1 "Prooemium")
                                              (push (list (pop files)
                                                          (format nil "~a ~a"
                                                                  (second it)
                                                                  (third it)))
                                                    files)
                                              (setf counter 0)
                                              (incf question-counter)))
                                        (:div.pr :id id
                                                 (elt (fwoar.string-utils:split " " (fourth it) :count 5) 5)))
                                       ((cl-ppcre:scan "ad [0-9]" (fourth it))
                                        (case state
                                          ((:arg :sc)
                                           (princ "</ol>" spinneret:*html*))
                                          (:corp
                                           (princ "</section>" spinneret:*html*)))
                                        (unless (eql state :ad)
                                          (princ "<ol class=\"ad\">" spinneret:*html*)
                                          (setf state :ad))
                                        (:li.adr :id id
                                                 (elt (fwoar.string-utils:split " " (fourth it) :count 8) 8)))
                                       (t (fourth it))))))
                                forms)
                       (case state
                         ((:arg :sc :ad)(princ "</ol>" spinneret:*html*))
                         (:corp (princ "</section>" spinneret:*html*)))
                       nil))))))
      (close *standard-output*)
      (setf *standard-output* old-s-o))
    (let ((*standard-output* (open (merge-pathnames "index.html" dir)
                                   :direction :output
                                   :if-exists :supersede)))
      (spinneret:with-html
        (:doctype html)
        (:link :rel "stylesheet"
               :href "../style.css")
        (:meta :charset "utf-8")
        (:main
         (:nav (:a :href ".." "totum"))
         (:ul
          (loop for (file cite) in (reverse files)
                do (:li (:a :href file cite)))))))))

(defun idify (ref)
  (remove-if-not 'alphanumericp
                 (format nil "~{~a~}" ref)))
(defun fnify (ref)
  (with-output-to-string (s)
    (mapcar (lambda (it)
              (princ (remove-if-not 'alphanumericp
                                    (typecase it
                                      (integer (format nil "~4,'0d" it))
                                      (t (format nil "~a" it))))
                     s))
            ref)))

(defun classify (context)
  (:printv context)
  "")
(defun htmlify (h-t type number title &optional context)
  (flet ((article-template (thing)
           (spinneret:with-html
             (let ((arg (gethash "arg" thing)))
               (when arg
                 (:h4 "Objectiones")
                 (let ((arg-v (ht->vector arg)))
                   (:ol.objectiones
                    (map nil
                         (lambda (it)
                           (:li.objectio :id (idify (car it))
                                         (cadr it)))
                         arg-v)))))
             (let ((sc (gethash "sc" thing)))
               (when sc
                 (:h4 "Sed Contra")
                 (typecase sc
                   (hash-table (let ((sc-v (ht->vector sc)))
                                 (:ol.sedcontrae
                                  (map nil
                                       (lambda (it)
                                         (:li.sedcontra :id (idify (car it))
                                                        (cadr it)))
                                       sc-v))))
                   (cons
                    (:ol.sedcontrae
                     (:li.sedcontra
                      :id (idify (car sc))
                      (cadr sc)))))))
             (let ((co (gethash "co" thing)))
               (when co
                 (:h4 "Respondeo")
                 (:section.corpus :id (idify (car co))
                                  (cadr co))))
             (let ((responsiones (gethash "ad" thing)))
               (when responsiones
                 (:h4 "Ad Objectiones")
                 (let ((responsiones-v (ht->vector responsiones)))
                   (:ol.responsiones
                    (map nil
                         (lambda (it)
                           (:li.responsio :id (idify (car it))
                                          (cadr it)))
                         responsiones-v)))))
             (let ((adarg (gethash "adarg" thing)))
               (when adarg
                 (:h4 "Ad Objectiones")
                 (:ol.responsiones
                  (:li.responsi :id (idify (car adarg))
                                (cadr adarg))))))))
    (let ((thing (gethash number h-t)))
      (string-case:string-case (type)
        ("q"
         (spinneret:with-html
           (let ((next-context (list* number "q" context)))
             (:section :id (idify (reverse next-context))
                       :class "quaestio"
                       (:h2 "Quaestio" (format nil "~@r" number) title
                            (:a :href (format nil "#~a" (idify (reverse next-context)))
                                "#"))
                       (let ((maybe-pr (gethash "pr" thing)))
                         (when maybe-pr
                           (:section.pr
                            (:h3 "Prooemium")
                            (destructuring-bind (ref pr) maybe-pr
                              (:div :id (idify ref)
                                    pr)))))
                       (let* ((article-ht (gethash "a" thing)))
                         (if article-ht
                             (:section.articuli
                              (let ((articles (ht->vector article-ht)))
                                (map nil
                                     (lambda (idx)
                                       (htmlify article-ht
                                                "a"
                                                idx
                                                ""
                                                next-context))
                                     (alexandria:iota (length articles) :start 1))))
                             (article-template thing)))
                       ))))
        ("a"
         (let ((next-context (list* number "a" context)))
           (spinneret:with-html
             (:section.articulus
              :id (idify (reverse next-context))
              (:h3 "Articulus" number title
                   (:a :href (format nil "#~a" (idify (reverse next-context)))
                       "#"))
              (article-template thing)
              ))))))))

(defun style.css ()
  (lass:compile-and-write
   '(:import (url "./counters.css"))
   '(:import (url "./fonts.css"))

   '((nav > a + a)
     :margin-left 1em)

   (let ((colors '(("zenburn-fg-plus-2" . "#FFFFEF")
                   ("zenburn-fg-plus-1" . "#F5F5D6")
                   ("zenburn-fg" . "#DCDCCC")
                   ("zenburn-fg-1" . "#A6A689")
                   ("zenburn-fg-2" . "#656555")
                   ("zenburn-black" . "#000000")
                   ("zenburn-bg-2" . "#000000")
                   ("zenburn-bg-1" . "#111112")
                   ("zenburn-bg-05" . "#383838")
                   ("zenburn-bg" . "#2A2B2E")
                   ("zenburn-bg-plus-05" . "#494949")
                   ("zenburn-bg-plus-1" . "#4F4F4F")
                   ("zenburn-bg-plus-2" . "#5F5F5F")
                   ("zenburn-bg-plus-3" . "#6F6F6F")
                   ("zenburn-red-plus-2" . "#ECB3B3")
                   ("zenburn-red-plus-1" . "#DCA3A3")
                   ("zenburn-red" . "#CC9393")
                   ("zenburn-red-1" . "#BC8383")
                   ("zenburn-red-2" . "#AC7373")
                   ("zenburn-red-3" . "#9C6363")
                   ("zenburn-red-4" . "#8C5353")
                   ("zenburn-red-5" . "#7C4343")
                   ("zenburn-red-6" . "#6C3333")
                   ("zenburn-orange" . "#DFAF8F")
                   ("zenburn-yellow" . "#F0DFAF")
                   ("zenburn-yellow-1" . "#E0CF9F")
                   ("zenburn-yellow-2" . "#D0BF8F")
                   ("zenburn-green-5" . "#2F4F2F")
                   ("zenburn-green-4" . "#3F5F3F")
                   ("zenburn-green-3" . "#4F6F4F")
                   ("zenburn-green-2" . "#5F7F5F")
                   ("zenburn-green-1" . "#6F8F6F")
                   ("zenburn-green" . "#7F9F7F")
                   ("zenburn-green-plus-1" . "#8FB28F")
                   ("zenburn-green-plus-2" . "#9FC59F")
                   ("zenburn-green-plus-3" . "#AFD8AF")
                   ("zenburn-green-plus-4" . "#BFEBBF")
                   ("zenburn-cyan" . "#93E0E3")
                   ("zenburn-blue-plus-3" . "#BDE0F3")
                   ("zenburn-blue-plus-2" . "#ACE0E3")
                   ("zenburn-blue-plus-1" . "#94BFF3")
                   ("zenburn-blue" . "#8CD0D3")
                   ("zenburn-blue-1" . "#7CB8BB")
                   ("zenburn-blue-2" . "#6CA0A3")
                   ("zenburn-blue-3" . "#5C888B")
                   ("zenburn-blue-4" . "#4C7073")
                   ("zenburn-blue-5" . "#366060")
                   ("zenburn-magenta" . "#DC8CC3"))))
     `(:root
       ,@(loop for (name . color) in colors
               appending (list (alexandria:make-keyword (format nil "--~a" name))
                               (string-downcase color)))))
   '(* :box-sizing border-box)
   '((:or html body ol)
     :margin 0
     :padding 0)

   '(body
     :color (var --foreground)
     :background-color (var --background))

   '(body
     :--background (var --zenburn-bg)
     :--foreground (var --zenburn-fg)
     :--foreground-diminish (var --zenburn-fg-1)
     :--foreground-highlight (var --zenburn-fg-plus-1)
     :--accent (var --zenburn-orange)
     :--link (var --zenburn-blue)
     :--link-visited (var --zenburn-magenta))

   '(:media "(prefers-color-scheme: light)"
     (body
      :--background (var --zenburn-fg)
      :--foreground (var --zenburn-bg)
      :--foreground-diminish (var --zenburn-bg-1)
      :--foreground-highlight (var --zenburn-bg-plus-1)
      :--accent (var --zenburn-red-5)
      :--link (var --zenburn-blue-4)
      :--link-visited (var --zenburn-green-4)))


   '(:media "print"
     (body
      :--background white
      :--foreground black
      :--foreground-diminish "#444"
      :--foreground-highlight white
      :--accent "#888"
      :--link black
      :--link-visited black)
     (main
      :text-align justify))

   '(a
     :color (var --link))
   '((:and a :visited)
     :color (var --link-visited))
   '((:and a :hover)
     :outline thin solid "currentColor")

   '(main
     :font-family sans-serif
     :hyphens auto
     :text-rendering optimizeLegibility
     :font-feature-settings "'liga' on, 'onum' on"

     :line-height 1.5
     :letter-spacing 1px
     :width 100vw
     :padding-top 2rem
     :padding-bottom 2rem
     :padding-right (calc (- 100vw 40em))
     :padding-left 5.5rem
     :min-height 100vh)

   '(label
     :position relative
     :padding 0.25em
     :left 5.5rem
     :top 0.5rem)
   '((:and input (:= type "radio"))
     :display none)

   '(((:or
       main
       (:and label (:= for "century-supra"))) ~ main)
     :font-family "century_supra_a")
   '((:or
      (:and label (:= for "concourse"))
      ((:and :checked "#concourse") ~ main))
     :font-family "concourse_4")

   '((:or h1 h2 h3 h4 h5 h6)
     :color (var --accent)
     :font-family "century_supra_a"
     :margin 0.25rem 0 0.5rem 0)
   '((:or h4 h5 h6)
     :display none)

   `(:let ()
      ,@(loop for tag in '(h1 h2 h3 h4 h5 h6)
              for font-size = 2 then (/ (+ font-size 1) 2.0)
              collect `(,tag :font-size ,(format nil "~arem" font-size))))

   '(((:not (:or h1 h2 h3 h4 h5 h6)) + (:or h1 h2 h3 h4 h5 h6))
     :margin-top 2.5rem
     :margin-bottom 1rem)

   '(u
     :color (var --foreground-highlight)
     :text-decoration underline 0.1px)

   '((li + li)
     :margin-top 0.25em)

   '((:or .pr .articulus)
     :padding-bottom 3em)
   '((:or ol.objectiones ol.sedcontrae #|ol.responsiones|# section.corpus)
     :padding-bottom 2.5em)
   '(((:or ol.objectiones ol.sedcontrae ol.responsiones section.corpus) li + li)
     :margin-top 1.5em)

   '((:and li "::marker")
     :color (var --foreground-diminish))

   '(ol.objectiones
     :list-style outside objectio)
   '(ol.sedcontrae
     :list-style outside sedcontra)
   '(ol.responsiones
     :list-style outside responsio)

   '((:or .objectio .sedcontra .responsio .toc)
     :margin 0
     :padding 0)

   '(:media (:and screen "(max-width: 980px)")
     (body
      :font-size 24px
      :line-height 1.2em)
     (main
      :padding 0 1em)
     ((:or ol.objectiones ol.sedcontrae ol.responsiones)
      :list-style-position inside))))

(defun question-template (questions number &optional context)
  (let ((spinneret:*html-lang* "it")
        (prooemium (fw.lu:dive (list number "pr") questions)))
    (spinneret:with-html
      (:doctype html)
      (:html
       (:meta :charset "utf-8")
       (:meta :property "book:author" :content "Sancti Thomae de Aquino")
       (:meta :property "book:tag" :content "theology")
       (:meta :property "book:tag" :content "theologia")
       (:meta :property "book:tag" :content "thomism")
       (:meta :property "book:tag" :content "aquinas")
       (when prooemium
         (:meta :property "og:description"
                :content (cadr prooemium)))
       (:link :rel "stylesheet" :href "../style.css")
       (:title ("Quaestio ~@r — Summa Theologiae" number))

       (:input :type "radio" :name "font" :id "concourse" :checked t)
       (:label :for "concourse" "A")
       (:input :type "radio" :name "font" :id "century-supra")
       (:label :for "century-supra" "A")
       (:main
        (:nav (:a :href "." "pars")
              (:a :href ".." "totum"))
        (htmlify questions "q" number "" context))))))

(defun dump-questions (dir questions &optional context)
  (let ((q-v (ht->vector questions))
        (files ()))
    (map nil
         (lambda (idx)
           (let ((ref (reverse (list* idx "q" context))))
             (with-open-file (*standard-output* (merge-pathnames (make-pathname
                                                                  :name (fnify ref)
                                                                  :type "html")
                                                                 dir)
                                                :direction :output
                                                :if-exists :supersede)
               (question-template questions idx context))
             (push ref files)))
         (alexandria:iota (length q-v) :start 1))

    (alexandria:with-output-to-file (*standard-output* (merge-pathnames "index.html" dir)
                                                       :if-exists :supersede)
      (spinneret:with-html
        (:doctype html)
        (:link :rel "stylesheet" :href "../style.css")
        (:body
         (:main
          (:nav (:a :href ".." "totum"))
          (:ul
           (loop for file in (reverse files)
                 do (:li
                     (:a :href (format nil "./~a.html" (fnify file))
                         (format nil "~{~a~^ ~}" file)))))))))))

(defun make-toplevel-structure (dir)
  (alexandria:with-output-to-file (s (merge-pathnames "style.css" dir)
                                     :if-exists :supersede)
    (princ (style.css) s)
    (terpri s))

  (alexandria:with-output-to-file (s (merge-pathnames "counters.css" dir)
                                     :if-exists :supersede)
    (princ *counters-css* s))

  (alexandria:with-output-to-file (s (merge-pathnames "fonts.css" dir)
                                     :if-exists :supersede)
    (princ *fonts-css* s))

  (alexandria:with-output-to-file (*standard-output* (merge-pathnames "index.html" dir)
                                                     :if-exists :supersede)
    (spinneret:with-html
      (:doctype html)
      (:html
       (:head
        (:meta :charset "utf-8")
        (:title "Summa Theologiae")
        (:link :rel "stylesheet" :href "./style.css"))
       (:body
        (:main
         (:h1 "Summa Theologiae")
         (:h2 "Sancti Thomae de Aquino")
         (:ul
          (:li (:a :href "./I/index.html" "Prima Pars"))
          (:li "Secunda Pars"
               (:ul
                (:li (:a :href "./I-II/index.html" "Prima Pars Secundae Partis"))
                (:li (:a :href "./II-II/index.html" "Secunda Pars Secundae Partis"))))
          (:li (:a :href "./III/index.html" "Tertia Pars"))))))))
  (values))

(defun decode-ref (num &optional long)
  (multiple-value-bind (m r) (floor num 1000)
    (list* (ecase m
             (0 (if long "liber" "l"))
             (1 (if long "prooemium" "pr"))
             (2 (if long "quaestio" "q"))
             (3 (if long "articulus" "a"))
             (4 "arg")
             (5 "sc")
             (6 "co")
             (7 "ad")
             (8 "adarg"))
           (unless (= r 0)
             (list r)))))

(defun ref->id (ref)
  (with-output-to-string (s)
    (mapc (lambda (it)
            (destructuring-bind (type &optional v) it
              (string-case:string-case (type)
                ("l" (format s "~@r" v))
                (t (format s "~a~:[~;~:*~a~]" type v)))))
          ref)))
(defun write-question (s)
  (lambda ()))
;;("ST" "I" "q. 3" "a. 7" "s.c.")
