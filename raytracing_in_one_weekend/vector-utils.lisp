(in-package :fwoar.lisp-sandbox.1)

(defun deg2rad (deg)
  (/ (* deg pi)
     180.0d0))

(defun rand-min-max (min max)
  (+ min
     (* (- max min)
        (random 1.0d0))))

(defun rand-vec3 ()
  (vec3 (random 1.0d0)
        (random 1.0d0)
        (random 1.0d0)))

(defun rand-vec3-min-max (min max)
  (vec3 (rand-min-max min max)
        (rand-min-max min max)
        (rand-min-max min max)))

(declaim (inline vec+ vec* vec- vec/ random-in-unit-sphere negate))
(defun random-in-unit-sphere ()
  (loop for p = (rand-vec3-min-max -1.0d0 1.0d0)
        while (>= (length-squared p)
                  1)
        finally (return p)))

(defun random-unit-vector ()
  (unit-vector (random-in-unit-sphere)))

(defun vec+ (vec1 vec2)
  (declare (optimize (speed 3)))
  (fw.lu:vector-destructuring-bind (a b c) vec1
    (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
      (vec3 (+ a a1)
            (+ b b1)
            (+ c c1)))))
(define-compiler-macro vec+ (&whole whole vec1 vec2)
  (cond ((and (vectorp vec1)
              (vectorp vec2))
         (vec+ vec1 vec2))
        ((vectorp vec1)
         (alexandria:once-only (vec2)
           `(fw.lu:vector-destructuring-bind (a b c) ,vec1
              (vec3
               (+ a (aref ,vec2 0))
               (+ b (aref ,vec2 1))
               (+ c (aref ,vec2 2))))))
        ((vectorp vec2)
         (alexandria:once-only (vec1)
           `(fw.lu:vector-destructuring-bind (a b c) ,vec2
              (vec3
               (+ a (aref ,vec1 0))
               (+ b (aref ,vec1 1))
               (+ c (aref ,vec1 2))))))
        (t whole)))
(defun vec- (vec1 vec2)
  (declare (optimize (speed 3)))
  (fw.lu:vector-destructuring-bind (a b c) vec1
    (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
      (vec3 (- a a1)
            (- b b1)
            (- c c1)))))
(defun vec* (vec1 vec2)
  (declare (optimize (speed 3)))
  (etypecase vec1
    ((array * (3)) (fw.lu:vector-destructuring-bind (a b c) vec1
                     (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
                       (vec3 (* a a1)
                             (* b b1)
                             (* c c1)))))
    (double-float (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
                    (vec3 (* vec1 a1)
                          (* vec1 b1)
                          (* vec1 c1))))
    (single-float (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
                    (vec3 (* vec1 a1)
                          (* vec1 b1)
                          (* vec1 c1))))
    (number (fw.lu:vector-destructuring-bind (a1 b1 c1) vec2
              (vec3 (* vec1 a1)
                    (* vec1 b1)
                    (* vec1 c1))))))
(defun negate (vec)
  (fw.lu:vector-destructuring-bind (x y z) vec
    (vec3 (- x)
          (- y)
          (- z))))

(defun vec/ (vec it)
  (declare (optimize (speed 3)))
  (vec* (/ 1.0 it)
        vec))
(defun dot (u v)
  (fw.lu:vector-destructuring-bind (a1 b1 c1) u
    (fw.lu:vector-destructuring-bind (a2 b2 c2) v
      (+ (* a1 a2)
         (* b1 b2)
         (* c1 c2)))))
(defun cross (u v)
  (fw.lu:vector-destructuring-bind (a1 b1 c1) u
    (fw.lu:vector-destructuring-bind (a2 b2 c2) v
      (vec3 (- (* b1 c2)
               (* c1 b2))
            (- (* c1 a2)
               (* a1 c2))
            (- (* a1 b2)
               (* b1 a2))))))

(defun length-squared (v)
  (fw.lu:vector-destructuring-bind (x y z) v
    (+ (* x x)
       (* y y)
       (* z z))))
(defun vec-length (v)
  (sqrt (length-squared v)))
(defun unit-vector (v)
  (vec/ v
        (vec-length v)))

(declaim (inline near-zero))
(defun near-zero (vec)
  (fw.lu:vector-destructuring-bind (x y z) vec
    (let* ((s 1.0d-8))
      (and (< (abs x) s)
           (< (abs y) s)
           (< (abs z) s)))))

(defvar *color-depth* 255)

(defun clamp (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defvar *samples-per-pixel* 1)
(defun scale-to-8bit (color)
  (let ((scale (/ *samples-per-pixel*)))
    (flet ((scale-to-depth (c)
             (floor
              (* *color-depth*
                 (clamp (sqrt (* c scale))
                        0.0d0 0.999d0)))))
      (fwoar.lisputils:vector-destructuring-bind (r g b) color
        (let ((r (scale-to-depth r))
              (g (scale-to-depth g))
              (b (scale-to-depth b)))
          (vec3 r g b))))))
