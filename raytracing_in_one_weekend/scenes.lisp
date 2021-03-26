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

(defun refraction-scene2
    (&optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50) (rows nil rows-p))
  (let* ((world (list (sphere #( 0.0 0 -1.0) 0.1 #(0.5 0.5 0.0) (dielectric-material 1.5))
                      (sphere #( 0.0 0 -1.4) 0.2 #(0.5 0.5 0.0) (dielectric-material 1.7))
                      (sphere #( 0.0 0 -2.4) 0.8 #(0.5 0.5 0.0) (dielectric-material 1.9))
                      (sphere #(-0.0 0 -3.6) 1.0 #(0.5 0.5 0.0) (dielectric-material 2.1))
                      (sphere #(-0.0 0 -5.0) 1.2 #(0.5 0.5 0.0) (dielectric-material 2.3))
                      (sphere #(-0.0 0 -6.6) 1.4 #(0.5 0.5 0.0) (dielectric-material 2.5))
                      #+foo (sphere #(-0.0 0 -1) -0.2
                                    #(0 0 0)
                                    (metal-material #(0.859375d0 0.859375d0 0.796875d0)))
                      #+foo (sphere #(0.6 0.0 -1) 0.2
                                    #(0 0 0)
                                    (metal-material #(0.859375d0 0.859375d0 0.796875d0)))
                      #+foo (sphere #(0 1 -1) 0.2
                                    nil
                                    (lambda (rec ray world depth)
                                      (declare (ignore rec ray world depth))
                                      #(1.0d0 1.0d0 1.0d0)))
                      (sphere #(0 -1000.5 -1.5) 1000
                              #(0.5 0.5 0.5)
                              (lambertian-material #(0.8 0.0 0.0))))
                #+(or)
                (list
                 (sphere #(-0.0 0 -1) 0.2
                         #(0 0 0)
                         (metal-material
                          #(0.8 0.6 0.2)))
                 (sphere #(0 -100.5 -1.5) 100
                         #(0.5 0.5 0.5)
                         (lambertian-material #(0.8 0.8 0.0)))))
         (aspect-ratio 16/9)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (if rows-p
                         rows
                         (shuffle (loop for x from 0 to image-height collect x)))
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

(defvar *planets*
  '(:mercury 3031/7917
    :venus   7520/7917
    :earth   7917/7917
    :mars    4212/7917))

(defun scene-3
    (&optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50) (rows nil rows-p))
  (let* ((world (list (sphere #(-2.25 0 -2)
                              (* 0.5 (getf *planets* :mercury))
                              nil
                              (metal-material #(0.8 0.8 0)))
                      (sphere #(-0.75 0 -2)
                              (* 0.5 (getf *planets* :venus))
                              nil
                              (metal-material #(0.0 0.8 0)))
                      (sphere #(0.75 0 -2)
                              (* 0.5 (getf *planets* :earth))
                              nil
                              (metal-material #(0.0 0.0 0.8)))
                      (sphere #(2.25 -0 -2)
                              (* 0.5 (getf *planets* :mars))
                              nil
                              (metal-material #(0.8 0.0 0.0)))))

         (aspect-ratio 16/9)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (if rows-p
                         rows
                         (shuffle (loop for x from 0 to image-height collect x)))
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

(defun scene-4
    (&optional (*samples-per-pixel* 10) (image-width 400) (max-depth 50) (rows nil rows-p))
  (let* ((world (append (loop for ti from 0 by (/ pi 7)
                              for x from 0
                              repeat 20
                              collect
                              (sphere
                               (vector (- (* 2 (sin ti)) 1)
                                       (- (* 2 (cos ti)) 1)
                                       (+ -0.5 (* -0.3 x)))
                               0.25
                               nil
                               (case (random 3)
                                 (0 (metal-material (vector (/ (mod (* 20 x)
                                                                    256)
                                                               256d0)
                                                            (/ (mod (* 20 x)
                                                                    256)
                                                               256d0)
                                                            (/ (mod (* 20 x)
                                                                    256)
                                                               256d0))))
                                 (1 (lambertian-material (vector (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0)
                                                                 (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0)
                                                                 (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0))))
                                 (2 (dielectric-material (rand-min-max 1.4 2.5)
                                                         (rand-min-max 0 0.2))))))
                        (loop for ti from 0 by (/ pi 7)
                              for x from 0
                              repeat 20
                              collect
                              (sphere
                               (vector (- (sin (+ ti (/ pi 14))) 0.5)
                                       (- (cos (+ ti (/ pi 14))) 0.5)
                                       (+ -0.5 (* -0.3 x)))
                               0.25
                               nil
                               (case (random 3)
                                 (0 (metal-material (vector (/ (mod (* 20 x)
                                                                    256)
                                                               256d0)
                                                            (/ (mod (* 20 x)
                                                                    256)
                                                               256d0)
                                                            (/ (mod (* 20 x)
                                                                    256)
                                                               256d0))))
                                 (1 (lambertian-material (vector (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0)
                                                                 (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0)
                                                                 (/ (mod (* 20 x)
                                                                         256)
                                                                    256d0))))
                                 (2 (dielectric-material (rand-min-max 1.4 2.5)
                                                         (rand-min-max 0 0.2)))))))
                #+(or)
                (list
                 (sphere #(-0.0 0 -1) 0.2
                         #(0 0 0)
                         (metal-material
                          #(0.8 0.6 0.2)))
                 (sphere #(0 -100.5 -1.5) 100
                         #(0.5 0.5 0.5)
                         (lambertian-material #(0.8 0.8 0.0)))))
         (aspect-ratio 16/9)
         (image-height (* (floor (/ image-width aspect-ratio))))

         (camera (camera)))
    (let ((mailbox (sb-concurrency:make-mailbox)))
      (loop for j in (if rows-p
                         rows
                         (shuffle (loop for x from 0 to image-height collect x)))
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

#+(or)
((list 'quote
       (loop for ti from 0 by (/ pi 7)
             for x from 0
             repeat 20
             collect (list (vector (- (* 2 (sin ti)) 1)
                                   (- (* 2 (cos ti)) 1)
                                   (+ -0.2 (* -0.3 x)))
                           0.25
                           nil
                           (list (vector (/ (mod (* 20 x)
                                                 256)
                                            256d0)
                                         (/ (mod (* 20 x)
                                                 256)
                                            256d0)
                                         (/ (mod (* 20 x)
                                                 256)
                                            256d0))))))

 '((#(-1.0 1.0 -0.2) 0.25 NIL)
   (#(-0.13223252176488376d0 0.8019377358048383d0 -0.5) 0.25 NIL)
   (#(0.5636629649360596d0 0.2469796037174672d0 -0.8) 0.25 NIL)
   (#(0.9498558243636472d0 -0.5549581320873711d0 -1.1) 0.25 NIL)
   (#(0.9498558243636472d0 -1.4450418679126287d0 -1.4000001) 0.25 NIL)
   (#(0.5636629649360598d0 -2.2469796037174667d0 -1.7) 0.25 NIL)
   (#(-0.13223252176488354d0 -2.801937735804838d0 -2.0) 0.25 NIL)
   (#(-0.9999999999999998d0 -3.0d0 -2.3000002) 0.25 NIL)
   (#(-1.8677674782351161d0 -2.801937735804838d0 -2.6000001) 0.25 NIL)
   (#(-2.5636629649360594d0 -2.246979603717467d0 -2.9) 0.25 NIL)
   (#(-2.949855824363647d0 -1.4450418679126291d0 -3.2) 0.25 NIL)
   (#(-2.949855824363647d0 -0.5549581320873715d0 -3.5000002) 0.25 NIL)
   (#(-2.56366296493606d0 0.24697960371746674d0 -3.8000002) 0.25 NIL)
   (#(-1.8677674782351166d0 0.8019377358048381d0 -4.1) 0.25 NIL)
   (#(-1.0000000000000004d0 1.0d0 -4.4) 0.25 NIL)
   (#(-0.1322325217648842d0 0.8019377358048385d0 -4.7) 0.25 NIL)
   (#(0.5636629649360592d0 0.2469796037174674d0 -5.0) 0.25 NIL)
   (#(0.949855824363647d0 -0.5549581320873707d0 -5.3) 0.25 NIL)
   (#(0.9498558243636475d0 -1.4450418679126282d0 -5.6) 0.25 NIL)
   (#(0.56366296493606d0 -2.2469796037174667d0 -5.9) 0.25 NIL))

 (/ 1960 60.0)
 )
