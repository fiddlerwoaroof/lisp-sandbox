(in-package :fwoar.lisp-sandbox.material)

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
