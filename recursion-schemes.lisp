(defpackage :fwoar.recursion-schemes
  (:use :cl)
  (:export ))
(in-package :fwoar.recursion-schemes)

(defun cdr* (cons)
  (funcall (cdr cons)))

;; cata :: (a -> b -> b) -> b -> [a] -> b
(defun cata (fun init as)
  (if (null as)
      init
      (funcall fun
               (car as)
               (cata fun init
                     (cdr as)))))

;; para :: (a -> [a] -> b -> b) -> b -> [a] -> b
(defun para (fun init as)
  (if (null as)
      init
      (funcall fun (car as) (cdr as)
               (para fun init (cdr as)))))

;; ana :: (v -> (a, () -> b)) -> b -> [a]
(defun ana (fun init)
  (destructuring-bind (a init*)
      (funcall fun init)
    (cons a (lambda () (ana fun init*)))))

;; ana :: (v -> Maybe (a, b)) -> b -> [a]
(defun ana* (fun init)
  (let ((v (funcall fun init)))
    (when v
      (cons (car v)
            (ana* fun (cadr v))))))

;; hylo  :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
(defun hylo (reducer reducer-init generator generator-seed)
  (flet ((partial-ana (init)
           (ana* generator init)))
    (cata reducer reducer-init
          (partial-ana generator-seed))))

;; zygo (a -> b -> b) -> (a -> b -> c -> c) -> b -> c -> [a] -> c
(defun zygo (fold-1 fold-2 fold-1-init fold-2-init as)
  (second
   (cata (fw.lu:destructuring-lambda (x (p q))
           (list (funcall fold-1 x p)
                 (funcall fold-2 x p q)))
         (list fold-1-init fold-2-init)
         as)))
