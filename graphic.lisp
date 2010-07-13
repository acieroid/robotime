(in-package robotime)

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
(defparameter *malus-color* (uid:make-color :red 1 :green 0 :blue 0 :alpha 0.4)
  "The background color of a malus")

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

(defgeneric draw (item)
  (:documentation "Draw an item on the board"))

(defmethod item-position ((item graphic-item))
  (list (x item) (y item)))

(defmethod (setf item-position) (pos (item graphic-item))
  (setf (x item) (first pos))
  (setf (y item) (second pos)))

(defun case= (a b)
  (and (= (first a) (first b))
       (= (second a) (second b))))

(defun case+ (a b)
  (list (+ (first a) (first b))
        (+ (second a) (second b))))

(defmethod pos= ((a graphic-item) (b graphic-item))
  (and (= (x a) (x b))
       (= (y a) (y b))))

(defun in-board (c)
  "return T if the case C is in the board"
  (and (>= (first c) 0) (>= (second c) 0)
       (< (first c) *n-cases*) (< (second c) *n-cases*)))


;; Isometric tiles
(defparameter *tile-width* 32)
(defparameter *tile-height* (/ *tile-width* 4))
(defparameter *n-cases-x* 16)
(defparameter *n-cases-y* 32)
(defparameter *ressources-dir* #p"/home/quentin/robotime/ressources/")

(defun load-image (name)
  (make-instance 'uid:image
                 :texture-filepath (merge-pathnames *ressources-dir*
                                                    name)))
(defun draw-at (x y drawable)
  (uid:draw drawable
            :x (+ (* x *tile-width*)
                  (if (oddp y) (/ *tile-width* 2) 0))
            :y (* y *tile-height*)))
