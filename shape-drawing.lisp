
(defclass shape ()
  ())

(defclass positioned-obect ()
  ((%by :accessor by :initarg :by :initform 0)
   (%lx :accessor lx :initarg :lx :initform 0)
   (%shape :accessor shape :initarg :shape
           :initform (error "a positioned object needs a shape"))))

(defgeneric lx (shape))
(defgeneric by (shape))
(defgeneric width (shape))
(defgeneric height (shape))

(defgeneric bounding-box (shape)
  (:documentation "Get the bounding box for a shape return a pair (#(LX BY) . #(W H))"))

(defclass rect ()
  ((%width :accessor width :initarg width :initform 0)
   (%height :accessor height :initarg height :initform 0)))

(defmethod bounding-box ((rect rect))
  (cons (vector (lx rect) (by rect))
        (vector (width rect) (height rect))))
