(defpackage :fwoar.lisp-sandbox.1
  (:use :cl
        )
  (:export ))
(in-package :fwoar.lisp-sandbox.1)

(define-symbol-macro infinity
  #.sb-ext:double-float-positive-infinity)
(defun deg2rad (deg)
  (/ (* deg pi)
     180.0d0))

(defstruct (size (:type vector))
  width height)
(defstruct (color (:type vector))
  r g b)
(defstruct (vec3 (:type vector)
                 (:constructor vec3 (x y z))
                 (:conc-name v3-))
  x y z)

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

(defun call-with-ppm-header (stream size callback &optional (colors 255))
  (format stream "P3~%~d ~d~%~d~%"
          (size-width size)
          (size-height size)
          colors)
  (funcall callback stream))

#.(progn (defmacro ig (&rest syms) `'(declare (ignore ,@syms)))
         nil)

(defvar *color-depth* 255)
(defun format-color (s v _ __)
  #.(ig _ __)
  (fwoar.lisputils:vector-destructuring-bind (r g b) v
    (format s "~4d ~4d ~4d"
            (round (* *color-depth* r))
            (round (* *color-depth* g))
            (round (* *color-depth* b)))))

(defun write-colors (stream colors columns)
  (let ((intermediate ())
        (idx 0))
    (funcall colors
             (lambda (color)
               (push color intermediate)
               (when (= (1- columns)
                        (mod idx columns))
                 (format stream "~{~/fwoar.lisp-sandbox.1::format-color/~^  ~}~&" intermediate)
                 (setf intermediate ()))
               (incf idx)))
    (when intermediate
      (format stream "~{~/fwoar.lisp-sandbox.1::format-color/~^  ~}~&" intermediate))))

(fw.lu:defclass+ ray ()
  ((origin :initarg :origin)
   (direction :initarg :direction)))

(defgeneric at (self it)
  (:method ((ray ray) (it number))
    (with-slots (origin direction) ray
      (vec+ origin
            (vec* it direction)))))

(fw.lu:defclass+ hit-record ()
  ((p :initarg :p :reader .p)
   (time :initarg :time :reader .time)
   (normal :initarg :normal :accessor .normal :initform ())
   (front-face :initarg :front-face :accessor .front-face :initform ())))
(defun set-face-normal (hit-record r outward-normal)
  (prog1 hit-record
    (with-slots (direction) r
      (let ((front-face (< (dot direction outward-normal)
                           0)))
        (setf (.front-face hit-record) front-face
              (.normal hit-record) (if front-face
                                       outward-normal
                                       (vec* -1 outward-normal)))))))

(defclass hittable ()
  ())
(fw.lu:defclass+ sphere (hittable)
  ((center :initarg :center)
   (radius :initarg :radius)))
(defgeneric hit (thing ray t-min t-max)
  (:method ((things list) (r ray) (t-min real) (t-max real))
    (let (temp-rec
          (hit-anything nil)
          (closest-so-far t-max))
      (loop for thing in things
            for (hit-p hit-rec) = (multiple-value-list
                                   (hit thing r t-min closest-so-far))
            when hit-p do
              (setf hit-anything t
                    closest-so-far (.time hit-rec)
                    temp-rec hit-rec))
      (when hit-anything
        (values hit-anything
                temp-rec))))
  (:method ((sphere sphere) (r ray) (t-min real) (t-max real))
    (uiop:nest (with-slots ((%center center) (%radius radius)) sphere)
               (let ((center %center) (radius %radius)))
               (with-slots ((%origin origin) (%direction direction)) r)
               (let ((origin %origin) (direction %direction)))
               (let* ((oc (vec- origin center))
                      (a (length-squared direction))
                      (half-b (dot oc direction))
                      (c (- (length-squared oc)
                            (* radius radius)))
                      (discriminant (- (* half-b half-b)
                                       (* a c))))
                 (if (< discriminant 0)
                     (return-from hit nil)
                     (let* ((sqrtd (sqrt discriminant))
                            (root (/ (- (- half-b)
                                        sqrtd)
                                     a)))
                       (when (or (< root t-min)
                                 (< t-max root))
                         (setf root (/ (- sqrtd half-b)
                                       a))
                         (when (or (< root t-min)
                                   (< t-max root))
                           (return-from hit nil)))
                       (let* ((p (at r root))
                              (outward-normal (vec/ (vec- p center)
                                                    radius)))

                         (values t
                                 (set-face-normal (hit-record p root)
                                                  r
                                                  outward-normal)))))))))

(defun hit-sphere (center radius r)
  (with-slots (origin direction) r
    (let* ((oc (vec- origin center))
           (a (length-squared direction))
           (half-b (dot oc direction))
           (c (- (length-squared oc)
                 (expt radius 2)))
           (discriminant (- (* half-b half-b)
                            (* a c))))
      (if (< discriminant 0)
          -1.0d0
          (/ (- (- half-b) (sqrt discriminant))
             a)))))

(defgeneric ray-color (ray world)
  (:method ((ray ray) world)
    (multiple-value-bind (hit-p rec)
        (hit world ray 0 infinity)
      (when hit-p
        (return-from ray-color
          (vec* 0.5
                (vec+ #(1 1 1)
                      (.normal rec)))))
      (with-slots (direction) ray
        (let* ((unit-direction (unit-vector direction))
               (it (+ (* 0.5 (v3-y unit-direction))
                      1.0d0)))
          (vec+ (vec* (- 1.0d0 it)
                      #(1.0d0 1.0d0 1.0d0))
                (vec* it
                      #(0.5d0 0.7d0 1.0d0))))))))

(defun raytrace (out)
  (let* ((world (list (sphere #(0 0 -1) 0.5)
                      (sphere #(0 -100.5 -1) 100)))
         (aspect-ratio (/ 16.0d0 9.0d0))
         (image-width 400)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (viewport-height 2.0d0)
         (viewport-width (* aspect-ratio viewport-height))
         (focal-length 1.0d0)

         (origin (vec3 0 0 0))
         (horizontal (vec3 viewport-width 0 0))
         (vertical (vec3 0 viewport-height 0))
         (lower-left-corner (vec- (vec- (vec- origin
                                              (vec/ horizontal 2))
                                        (vec/ vertical 2))
                                  (vec3 0 0 focal-length))))
    (alexandria:with-output-to-file (s out :if-exists :supersede)
      (call-with-ppm-header s (make-size :width image-width :height image-height)
                            (lambda (s)
                              (write-colors s
                                            (lambda (c)
                                              (loop for j from (1- image-height) downto 0
                                                    do (format *trace-output*
                                                               "~&Scanlines remaining: ~d ~s~%"
                                                               j
                                                               (local-time:now))
                                                    do
                                                       (loop for i from 0 below image-width
                                                             for u = (/ (* 1.0d0 i)
                                                                        (1- image-width))
                                                             for v = (/ (* 1.0d0 j)
                                                                        (1- image-height))
                                                             for r = (ray origin
                                                                          (vec- (vec+ (vec+ lower-left-corner
                                                                                            (vec* u
                                                                                                  horizontal))
                                                                                      (vec* v
                                                                                            vertical))
                                                                                origin))
                                                             for color = (ray-color r world)
                                                             collect
                                                             (funcall c color))))
                                            image-width))
                            (round *color-depth*)))))

(defun sample-image (out)
  (let ((image-width 256)
        (image-height 256))
    (alexandria:with-output-to-file (s out :if-exists :supersede)
      (call-with-ppm-header s (make-size :width image-width :height image-height)
                            (lambda (s)
                              (write-colors s
                                            (lambda (c)
                                              (loop for j from (1- image-height) downto 0
                                                    do (format *trace-output*
                                                               "~&Scanlines remaining: ~d ~s~%"
                                                               j
                                                               (local-time:now))
                                                    do
                                                       (loop for i from 0 below image-width
                                                             collect
                                                             (let* ((r (/ (* i 1.0d0)
                                                                          (1- image-width)))
                                                                    (g (/ (* j 1.0d0)
                                                                          (1- image-height)))
                                                                    (b 0.15d0))
                                                               (funcall c (make-color :r r
                                                                                      :g g
                                                                                      :b b))))))
                                            image-width))
                            (1- #.(expt 2 8))))))
