(in-package :fwoar.lisp-sandbox.1)

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

(fw.lu:defclass+ ray ()
  ((origin :initarg :origin)
   (direction :initarg :direction)))

(fw.lu:defclass+ hit-record ()
  ((p :initarg :p :reader .p)
   (time :initarg :time :reader .time)
   (thing :initarg :thing :accessor .thing)
   (material :initarg :material :accessor .material)
   (normal :initarg :normal :accessor .normal :initform ())
   (front-face :initarg :front-face :accessor .front-face :initform ())))

(defclass camera ()
  ((origin :initarg :origin :reader origin)
   (lower-left-corner :initarg :lower-left-corner :reader lower-left-corner)
   (horizontal :initarg :horizontal :reader horizontal)
   (vertical :initarg :vertical :reader vertical)))

(defstruct (vec3 (:type vector)
                 (:constructor vec3 (x y z))
                 (:conc-name v3-))
  x y z)

(defun call-with-ppm-header (stream size callback &optional (colors 255))
  (format stream "P3~%~d ~d~%~d~%"
          (size-width size)
          (size-height size)
          colors)
  (funcall callback stream))

#.(progn (defmacro ig (&rest syms) `'(declare (ignore ,@syms)))
         nil)

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

(defgeneric at (self it)
  (:method ((ray ray) (it number))
    (with-slots (origin direction) ray
      (vec+ origin
            (vec* it direction)))))
(defun set-face-normal (hit-record r outward-normal)
  (prog1 hit-record
    (with-slots (direction) r
      (let ((front-face (< (dot direction outward-normal)
                           0)))
        (setf (.front-face hit-record) front-face
              (.normal hit-record) (if front-face
                                       outward-normal
                                       (vec* -1 outward-normal)))))))

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

(defun reflectance (cosine ref-idx)
  (let* ((r0 (/ (- 1 ref-idx)
                (+ 1 ref-idx)))
         (r0 (* r0 r0)))
    (+ r0
       (* (- 1 r0)
          (expt (- 1 cosine)
                5)))))

(define-symbol-macro infinity
  #.sb-ext:double-float-positive-infinity)
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
