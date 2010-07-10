;; TODO: sort settings/parameters/vars
(defparameter *font-size* 10)
(defparameter *font-color* uid:*blue*)
(defparameter *case-size* 16 "The size of a case of the board")
(defparameter *n-cases* 32 "The number of cases on the board")
(defparameter *entity-size* (/ *case-size* 2) "The size of an entity")
(defparameter *power-width* 16 "The width of the power indicator")
(defparameter *power-height* 100 "The maximal height of the power indicator")
(defparameter *power-color* uid:*blue* "The color of the power indicator")
(defparameter *player-color* uid:*blue* "The color of the player")
(defparameter *robot-color* uid:*red* "The color of the robots")
(defparameter *bonus-color* uid:*green* "The color of a bonus")

(defun position-for-case (x y size)
  (list (+ 1 (* x *case-size*) (/ (- *case-size* size) 2))
        (+ 1 (* y *case-size*) (/ (- *case-size* size) 2))))

(defmacro draw-rectangle-in-case (x y size &rest args)
  `(destructuring-bind (x y) (position-for-case ,x ,y ,size)
    (uid:draw-rectangle x y ,size ,size ,@args)))

(defun draw-letter-in-case (x y letter)
  (destructuring-bind (x y) (position-for-case x y *font-size*)
    (uid:draw (string letter) :x x :y y)))

(defun draw-power (x y value max-value)
  (uid:draw-rectangle x y *power-width*
                      (* (/ value max-value) *power-height*)
                      :color *power-color*)
  (uid:draw-rectangle x y *power-width* *power-height* :filledp nil))


(defclass graphic-item ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(defgeneric draw (entity)
  (:documentation "Draw an entity on the board"))

(defmethod pos= ((a graphic-item) (b graphic-item))
  (and (= (x a) (x b))
       (= (y a) (y b))))
