(defpackage :fwoar.lisp-sandbox.wordle
  (:use :cl )
  (:export ))
(in-package :fwoar.lisp-sandbox.wordle)

(defmacro define-printer ((class) &rest accessors)
  `(defmethod print-object ((o ,class) s)
     (format s "#<~a ~@{~s~}>"
             (class-name (class-of o))
             ,@(mapcar (lambda (it)
                         `(,it o))
                       accessors))))

(fw.lu:defclass+ guess-score ()
  ((%c :reader guess-char :initarg :guess-char)))
(define-printer (guess-score) guess-char)
(fw.lu:defclass+ miss ((guess-score (guess-char))) ())
(fw.lu:defclass+ exac ((guess-score (guess-char))) ())
(fw.lu:defclass+ misp ((guess-score (guess-char))) ())

(defun wordle-match (answer guess)
  (let* ((exact-matches (remove-if 'null (map 'list (lambda (a b) (when (char-equal a b) a))
                                              answer guess)))
         (wrong-spot (set-difference (intersection (coerce answer 'list)
                                                   (coerce guess 'list)
                                                   :test 'char-equal)
                                     exact-matches
                                     :test 'char-equal)))
    (coerce (loop for g across guess
                  for a across answer
                  collect (cond
                            ((char-equal g a) (exac g))
                            ((member g wrong-spot :test 'char-equal) (misp g))
                            (t (miss g))))
            'vector)))

(defun show-game (answer guesses)
  (loop for guess in guesses
        collect (wordle-match answer guess)))

(defun get-five-letter-words ()
  (with-open-file (s "/usr/share/dict/words")
    (loop for line = (read-line s nil)
          while line
          when (= (length line) 5)
            collect line)))

(defun matches-pattern (word pattern)
  (loop for w across word
        for p across pattern
        always (if (alpha-char-p p)
                   (char-equal w p)
                   t)))

(defun has-all-chars (word present-chars)
  (every (lambda (c)
           (position c word :test 'char-equal))
         present-chars))

(defun has-no-chars (word present-chars)
  (not (some (lambda (c)
               (position c word :test 'char-equal))
             present-chars)))

(defun search-words (wordlist pattern anti-patterns present-chars absent-chars)
  (loop for word in wordlist
        when (and (matches-pattern word pattern)
                  (not (some (lambda (anti-pattern)
                               (matches-pattern word anti-pattern))
                             anti-patterns))
                  (has-all-chars word present-chars)
                  (has-no-chars word absent-chars))
          collect word))
