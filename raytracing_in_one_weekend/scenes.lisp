(in-package :fwoar.lisp-sandbox.1)

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
