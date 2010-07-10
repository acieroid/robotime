(defparameter *case-size* 16 "The size of a case of the board")
(defparameter *n-cases* 32 "The number of cases on the board")

(defmacro draw-rectangle-in-case (x y size &rest args)
  `(uid:draw-rectangle (+ 1 (* ,x *case-size*) (/ (- *case-size* ,size) 2))
                       (+ 1 (* ,y *case-size*) (/ (- *case-size* ,size) 2))
                       ,size ,size
                       ,@args))

(defclass graphic-item ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(defgeneric draw (entity)
  (:documentation "Draw an entity on the board"))