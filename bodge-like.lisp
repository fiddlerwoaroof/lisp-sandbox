(defpackage :fwoar.bodge-like
  (:use :cl )
  (:export ))
(in-package :fwoar.bodge-like)

(defclass feature ()
  ((%sym :reader sym :initarg sym)))

(defclass monster (feature)
  ((%position :accessor pos :initform (gamekit:vec2 (random 12)
                                                    (random 12)))))

(defclass dog (monster)
  ()
  (:default-initargs sym "d"))

(defun monster (sym)
  (make-instance 'monster 'sym sym))

(defclass player (feature)
  ((%position :accessor pos :initform (gamekit:vec2 0 0)))
  (:default-initargs sym "@"))

(defparameter *white* (gamekit:vec4 1 1 1 1))
(defparameter *black* (gamekit:vec4 0 0 0 0.8))
(defparameter *tile-size* 32)

(gamekit:defgame bodgelike ()
  ((%player :reader player :initform (make-instance 'player))
   (%monsters :accessor monsters :initform ()))
  (:viewport-title "@")
  (:viewport-height (* 12 *tile-size*))
  (:viewport-width (* 12 *tile-size*)))

(defun draw-tile (row col &optional (size *tile-size*))
  (gamekit:draw-rect (gamekit:vec2 (* row size)
                                   (* col size))
                     size size
                     :fill-paint *black*))

(defvar *my-font*)
(defun draw-feature (feature &optional pos (tile-size *tile-size*))
  (let* ((pos (if pos
                  pos
                  (pos feature)))
         (row (gamekit:x pos))
         (col (gamekit:y pos))
         (tile-origin (gamekit:vec2 (* row tile-size)
                                    (* col tile-size)))
         (feature-text (etypecase feature
                         (string feature)
                         (feature (sym feature)))))
    (multiple-value-bind (text-origin text-width text-height)
        (gamekit:calc-text-bounds feature-text *my-font*)
      (let ((text-origin
              (gamekit:add tile-origin
                           (gamekit:mult -1 text-origin)
                           (gamekit:mult 0.5
                                         (gamekit:subt (gamekit:vec2 tile-size tile-size)
                                                       (gamekit:vec2 text-width text-height))))))
        (gamekit:draw-text feature-text text-origin
                           :fill-color *white*
                           :font *my-font*)))))


(gamekit:define-font :fantasy "/tmp/foo/font.ttf")


(defgeneric interact2 (game obj1 obj2)
  (:method (game obj1 obj2) nil)
  (:method (game obj1 obj2) t)
  (:method (game (obj1 player) (obj2 dog))
    (format t "~&Ruff! ~s~%" (gamekit:subt (pos obj2)
                                           (pos obj1)))
    (setf (pos obj2)
          (gamekit:add (pos obj2)
                       (gamekit:subt (pos obj2)
                                     (pos obj1))))
    nil))

(defun find-in-space (pos obj-list)
  (find pos obj-list 
        :key 'pos 
        :test 'bodge-math:vec=))

(defun update (thing fun &rest args)
  (apply fun thing args))

(define-modify-macro updatef (fun &rest args)
  update)

(defun move-handler (delta)
  (lambda (game player)
    (updatef (pos player)
             (lambda (old-pos)
               (let* ((new-pos (gamekit:add old-pos delta))
                      (feature-in-space (find-in-space new-pos (monsters game))))
                 (if (and feature-in-space (interact2 game player feature-in-space))
                     old-pos
                     new-pos))))) )

(defun move-left (game player)
  (let ((handler (move-handler (gamekit:vec2 -1 0))))
    (lambda ()
      (funcall handler game player))))

(defun move-right (game player)
  (let ((handler (move-handler (gamekit:vec2 1 0)))) 
    (lambda ()
      (funcall handler game player))))

(defun move-up (game player)
  (let ((handler (move-handler (gamekit:vec2 0 1)))) 
    (lambda ()
      (funcall handler game player))))

(defun move-down (game player)
  (let ((handler (move-handler (gamekit:vec2 0 -1)))) 
    (lambda ()
      (funcall handler game player))))


(defmethod gamekit:post-initialize ((game bodgelike))
  (gamekit:bind-button :h :pressed
                       (move-left game (player game)))
  (gamekit:bind-button :j :pressed
                       (move-down game (player game)))
  (gamekit:bind-button :k :pressed
                       (move-up game (player game)))
  (gamekit:bind-button :l :pressed
                       (move-right game (player game))))

(defmethod style )

(defmethod gamekit:draw ((system bodgelike))
  (dotimes (row 16)
    (dotimes (col 16)
      (draw-tile row col)))
  (let ((*my-font* (gamekit:make-font :fantasy (* 0.8 *tile-size*))))
    (mapcar 'draw-feature
            (monsters system))
    (draw-feature (player system))))
