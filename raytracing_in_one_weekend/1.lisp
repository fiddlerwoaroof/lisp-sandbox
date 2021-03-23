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

(defun rand-min-max (min max)
  (+ min
     (* (- max min)
        (random 1.0d0))))


(defstruct (size (:type vector))
  width height)
(defstruct (color (:type vector))
  r g b)
(defstruct (vec3 (:type vector)
                 (:constructor vec3 (x y z))
                 (:conc-name v3-))
  x y z)

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

(defun call-with-ppm-header (stream size callback &optional (colors 255))
  (format stream "P3~%~d ~d~%~d~%"
          (size-width size)
          (size-height size)
          colors)
  (funcall callback stream))

#.(progn (defmacro ig (&rest syms) `'(declare (ignore ,@syms)))
         nil)

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

(defun format-color (s v _ __)
  #.(ig _ __)
  (fw.lu:vector-destructuring-bind (r g b) (scale-to-8bit v)
    (format s "~4d ~4d ~4d" r g b)))

(defun write-colors (stream colors columns)
  (funcall colors
           (lambda (color pos)
             (fwoar.lisp-sandbox.canvas-server:send-update
              (scale-to-8bit color) pos)))
  #+(or)
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
   (thing :initarg :thing :accessor .thing)
   (material :initarg :material :accessor .material)
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
   (radius :initarg :radius)
   (material-color :initarg :color
                   :reader material-color
                   :initform (vec3 (random 1.0d0)
                                   (random 1.0d0)
                                   (random 1.0d0)))
   (material :initarg :material
             :reader .material
             :initform (lambertian-material (random 0.8)))))

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
    (declare (optimize (speed 3)))
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
                                 (set-face-normal (hit-record p root sphere (.material sphere))
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

(defgeneric scatter (material ray-in rec))

(defun original-material (rec ray world depth)
  (declare (ignore ray world depth))
  (vec* 0.5
        (vec+ #(1 1 1)
              (.normal rec))))

(defun lambertian-material (albedo)
  (lambda (rec ray world depth)
    (declare (ignore ray))
    (let ((scatter-direction (vec+ (.normal rec)
                                   (random-unit-vector))))
      (when (near-zero scatter-direction)
        (setf scatter-direction (.normal rec)))

      (vec* albedo
            (ray-color (ray (.p rec)
                            scatter-direction)
                       world
                       (1- depth))))))

(defun reflect (v n)
  (vec- v
        (vec* 2.0d0
              (vec* (dot v n)
                    n))))

(defun refract (uv n eta*/eta)
  (let* ((cos-theta (min 1.0d0
                         (dot (negate uv)
                              n)))
         (out-perp (vec* eta*/eta
                         (vec+ uv
                               (vec* cos-theta
                                     n))))
         (out-parallel (vec* (- (sqrt (abs (- 1.0d0
                                              (length-squared out-perp)))))
                             n)))
    (vec+ out-perp out-parallel)))

(defun metal-material (albedo)
  (lambda (rec ray world depth)
    (with-slots (direction) ray
      (let* ((reflected (reflect (unit-vector direction)
                                 (.normal rec)))
             (scattered (ray (.p rec) reflected)))
        (vec* albedo
              (ray-color scattered
                         world
                         (1- depth)))))))

(defun fuzzy-metal-material (albedo fuzz)
  (lambda (rec ray world depth)
    (with-slots (direction) ray
      (let* ((reflected (reflect (unit-vector direction)
                                 (.normal rec)))
             (scattered (ray (.p rec)
                             (vec+ reflected
                                   (vec* fuzz
                                         (random-in-unit-sphere))))))
        (vec* albedo
              (ray-color scattered
                         world
                         (1- depth)))))))

(defun reflectance (cosine ref-idx)
  (let* ((r0 (/ (- 1 ref-idx)
                (+ 1 ref-idx)))
         (r0 (* r0 r0)))
    (+ r0
       (* (- 1 r0)
          (expt (- 1 cosine)
                5)))))

(defun dielectric-material (ir &optional (fuzz 0))
  (lambda (rec ray world depth)
    (with-slots (direction) ray
      (let* ((attenuation (vec3 1.0d0 1.0d0 1.0d0))
             (refraction-ratio (if (.front-face rec)
                                   (/ 1.0d0 ir)
                                   ir))
             (unit-direction (unit-vector direction))
             (normal (.normal rec))
             (cos-theta (min 1.0d0
                             (dot (negate unit-direction)
                                  normal)))
             (sin-theta (sqrt (- 1
                                 (* cos-theta cos-theta))))
             (cannot-refract (> (* refraction-ratio
                                   sin-theta)
                                1.0d0))
             (direction (if (or cannot-refract
                                (> (reflectance cos-theta
                                                refraction-ratio)
                                   (random 1.0d0)))
                            (reflect unit-direction
                                     normal)
                            (refract unit-direction
                                     normal
                                     refraction-ratio)))
             (scattered (ray (.p rec)
                             (vec+ direction
                                   (vec* fuzz
                                         (random-in-unit-sphere))))))
        (vec* attenuation
              (ray-color scattered
                         world
                         (1- depth)))))))

(defgeneric ray-color (ray world depth)
  (:method :around (r w (depth integer))
    (if (<= depth 0)
        (vec3 1.0d0 1.0d0 1.0d0)
        (call-next-method)))
  (:method ((ray ray) world (depth integer))
    (multiple-value-bind (hit-p rec)
        (hit world ray 0.001d0 infinity)
      (when hit-p
        (return-from ray-color
          (funcall (.material rec) rec ray world depth)))
      (with-slots (direction) ray
        (let* ((unit-direction (unit-vector direction))
               (it (+ (* 0.5 (v3-y unit-direction))
                      1.0d0)))
          (vec+ (vec* (- 1.0d0 it)
                      #(1.0d0 1.0d0 1.0d0))
                (vec* it
                      #(0.5d0 0.7d0 1.0d0))))))))


(defclass camera ()
  ((origin :initarg :origin :reader origin)
   (lower-left-corner :initarg :lower-left-corner :reader lower-left-corner)
   (horizontal :initarg :horizontal :reader horizontal)
   (vertical :initarg :vertical :reader vertical)))

(defun camera (&key
                 (aspect-ratio 16/9)
                 (viewport-height 2.0d0)
                 (viewport-width (* aspect-ratio viewport-height))
                 (focal-length 1.0d0))
  (let ((origin (vec3 0d0 0.0d0 0.0d0))
        (horizontal (vec3 viewport-width 0.0d0 0.0d0))
        (vertical (vec3 0.0d0 viewport-height 0.0d0)))
    (make-instance 'camera
                   :origin origin
                   :horizontal horizontal
                   :vertical vertical
                   :lower-left-corner (vec- (vec- (vec- origin
                                                        (vec/ horizontal 2))
                                                  (vec/ vertical 2))
                                            (vec3 0 0 focal-length)))))
(defgeneric get-ray (camera u v)
  (:method ((camera camera) (u real) (v real))
    (with-slots (origin horizontal vertical lower-left-corner) camera
      (macrolet ((-> (v &body forms)
                   (if forms
                       `(-> (,(caar forms) ,v ,@(cdar forms))
                            ,@(cdr forms))
                       v)))


        (ray origin
             (-> lower-left-corner
                 (vec+ (vec* u horizontal))
                 (vec+ (vec* v vertical))
                 (vec- origin)))))))

(declaim (notinline u-loop))
(defun u-loop (j image-width image-height camera world max-depth c)
  (loop for i from 0 below image-width
        for u = (/ (* 1.0d0 i)
                   (1- image-width))
        for v = (/ (* 1.0d0 j)
                   (1- image-height))
        for r = (get-ray camera u v)
        for color = (loop for s below *samples-per-pixel*
                          for u = (/ i
                                     (1- image-width))
                            then (/ (+ i (random 1.0d0))
                                    (1- image-width))
                          for v = (/ j
                                     (1- image-height))
                            then (/ (+ j (random 1.0d0))
                                    (1- image-height))
                          for r = (get-ray camera u v)
                          for pixel-color = (ray-color r world max-depth)
                            then (vec+ pixel-color
                                       (ray-color r world max-depth))
                          finally (return pixel-color))
        do (funcall c color
                    (list i
                          (- image-height j)))))

(defvar *thread-queues*
  (make-array 8))

(defun start-worker (id)
  (let ((mailbox (sb-concurrency:make-mailbox :name (format nil "mailbox-~d" id))))
    (setf (aref *thread-queues* id) mailbox)
    (bt:make-thread
     (lambda ()
       (loop
         (destructuring-bind (*samples-per-pixel*
                              j image-width image-height camera world max-depth c)
             (sb-concurrency:receive-message mailbox)
           (u-loop j image-width image-height camera world max-depth c))))
     :name (format nil "worker-~d" id))))

(defun start-workers (&optional (n 8))
  (loop for x below n
        collect (start-worker x)))

(defun shuffle (seq)
  (let ((arr (map 'vector 'identity seq)))
    (loop for i from (1- (length arr)) downto 1
          for j = (random i)
          do (rotatef (aref arr i)
                      (aref arr j))
          finally (return (coerce arr (type-of seq))))))

(defun raytrace (out &optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50))
  (let* ((world (append (list (sphere #(0 0 -1) .5
                                      #(0.5 0.5 0.0)
                                      (dielectric-material 0.4)
                                      #+(or)
                                      (lambda (rec ray world depth)
                                        (declare (ignore rec world depth))
                                        (with-slots (direction) ray
                                          (fw.lu:vector-destructuring-bind
                                              (x y z) (unit-vector direction)
                                            (vector (abs x)
                                                    (abs y)
                                                    (abs z)))))))
                        (loop repeat 30
                              collect (sphere (vector (rand-min-max -3 3)
                                                      (rand-min-max -0.5 2)
                                                      (rand-min-max -1.5d0 -0.5d0))
                                              (rand-min-max 0.2 0.5)
                                              (vector (random 1.0d0)
                                                      (random 1.0d0)
                                                      (random 1.0d0))
                                              (case (random 5)
                                                (0 (lambertian-material
                                                    (rand-min-max 0.25 0.75)))
                                                (1 (metal-material
                                                    (rand-min-max 0.25 0.75)))
                                                (2 (fuzzy-metal-material
                                                    (rand-min-max 0.25 0.75)
                                                    (rand-min-max 0.01 0.75)))
                                                (3 (dielectric-material
                                                    (rand-min-max 0.1 2.4)))
                                                (4 #'original-material)
                                                (5
                                                 (lambda (rec ray world depth)
                                                   (declare (ignore rec world depth))
                                                   (vector 1d0 1d0 1d0)
                                                   #+(or)
                                                   (with-slots (direction) ray
                                                     (fw.lu:vector-destructuring-bind
                                                         (x y z) (unit-vector direction)
                                                       (vector (abs x)
                                                               (abs y)
                                                               (abs z)))))))))
                        ;; #+(or)
                        (list (sphere #(0 -100.5 -1) 100
                                      #(0.5 0.5 0.5)
                                      (lambertian-material 0.2)))))
         (aspect-ratio 4/3)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera :focal-length 1.0d0)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (shuffle (loop for x from 0 to image-height collect x))
            do
               (sb-concurrency:send-message
                (aref *thread-queues*
                      (mod j
                           (length *thread-queues*)))
                (list *samples-per-pixel*
                      j image-width
                      image-height camera
                      world max-depth
                      (lambda (a b)
                        (sb-concurrency:send-message
                         mailbox
                         (list a b))))))
      (write-colors nil
                    (lambda (c)
                      (loop with messages = 0
                            for it = (sb-concurrency:receive-message mailbox :timeout 2)
                            while it
                            do (destructuring-bind (color pos) it
                                 (funcall c color pos))))
                    image-width))))

(defun refraction-scene
    (out &optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50))
  (let* ((world (list (sphere #(0 0 -1) .5
                              #(0.5 0.5 0.0)
                              (lambertian-material #(0.7 0.3 0.3)))
                      (sphere #(-1 0 -1) 0.5
                              #(0 0 0)
                              (dielectric-material 1.5))
                      (sphere #(-1 0 -1) -0.4
                              #(0 0 0)
                              (dielectric-material 1.5))
                      (sphere #(-1 0 -1) -0.2
                              #(0 0 0)
                              (lambertian-material
                               #(0.8 0.6 0.2)))
                      #+(or)
                      (sphere #(1 0 -1) 0.5
                              #(0 0 0)
                              (dielectric-material 2.4))
                      #+(or)
                      (sphere #(-2 0 -1) 0.5
                              #(0 0 0)
                              (metal-material #(0.8 0.8 0.8)))
                      (sphere #(1 0 -1) 0.5
                              #(0 0 0)
                              (metal-material #(0.8 0.6 0.2)))
                      (sphere #(0 -100.5 -1) 100
                              #(0.5 0.5 0.5)
                              (lambertian-material #(0.8 0.8 0.0)))))
         (aspect-ratio 16/9)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (shuffle (loop for x from 0 to image-height collect x))
            do
               (sb-concurrency:send-message
                (aref *thread-queues*
                      (mod j
                           (length *thread-queues*)))
                (list *samples-per-pixel*
                      j image-width
                      image-height camera
                      world max-depth
                      (lambda (a b)
                        (sb-concurrency:send-message
                         mailbox
                         (list a b))))))
      (write-colors nil
                    (lambda (c)
                      (loop with messages = 0
                            for it = (sb-concurrency:receive-message mailbox :timeout 2)
                            while it
                            do (destructuring-bind (color pos) it
                                 (funcall c color pos))))
                    image-width))))

(defun refraction-scene1
    (out &optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50))
  (let* ((world (list (sphere #(0 0 -1) 1
                              #(0.5 0.5 0.0)
                              (dielectric-material 2.4))
                      (sphere #(-0.5 0 -1) 0.5
                              #(0 0 0)
                              (dielectric-material 1.5 0.2))
                      (sphere #(-0.5 0 -1) -0.4
                              #(0 0 0)
                              (dielectric-material 1.5 0.2))
                      (sphere #(-0.5 0 -1) -0.2
                              #(0 0 0)
                              (metal-material
                               #(0.8 0.6 0.2)))
                      #+(or)
                      (sphere #(1 0 -1) 0.5
                              #(0 0 0)
                              (dielectric-material 2.4))
                      #+(or)
                      (sphere #(-2 0 -1) 0.5
                              #(0 0 0)
                              (metal-material #(0.8 0.8 0.8)))
                      (sphere #(0.5 0 -1) 0.5
                              #(0 0 0)
                              (metal-material #(0.8 0.6 0.2)))
                      (sphere #(0 -100.5 -1) 100
                              #(0.5 0.5 0.5)
                              (lambertian-material #(0.8 0.8 0.0)))))
         (aspect-ratio 16/9)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (shuffle (loop for x from 0 to image-height collect x))
            do
               (sb-concurrency:send-message
                (aref *thread-queues*
                      (mod j
                           (length *thread-queues*)))
                (list *samples-per-pixel*
                      j image-width
                      image-height camera
                      world max-depth
                      (lambda (a b)
                        (sb-concurrency:send-message
                         mailbox
                         (list a b))))))
      (write-colors nil
                    (lambda (c)
                      (loop with messages = 0
                            for it = (sb-concurrency:receive-message mailbox :timeout 2)
                            while it
                            do (destructuring-bind (color pos) it
                                 (funcall c color pos))))
                    image-width))))

(defun sample-image (out)
  (let ((image-width 256)
        (image-height 256))
    (alexandria:with-output-to-file (s out :if-exists :supersede)
      (call-with-ppm-header
       s (make-size :width image-width :height image-height)
       (lambda (s)
         (write-colors
          s (lambda (c)
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
