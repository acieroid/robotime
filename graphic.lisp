(in-package robotime)

(defparameter *font-size* 10)
(defparameter *power-width* 16 "The width of the power indicator")
(defparameter *power-height* 100 "The maximal height of the power indicator")
(defparameter *power-color* uid:*blue* "The color of the power indicator")
(defparameter *tile-width* 32)
(defparameter *tile-height* (/ *tile-width* 4))
(defparameter *n-cases-x* 20)
(defparameter *n-cases-y* 80)
(defparameter *ressources-dir* #p"/home/quentin/robotime/ressources/")

(defun draw-power (x y value max-value)
  (uid:draw-rectangle x y *power-width*
                      (* (/ value max-value) *power-height*)
                      :color *power-color*)
  (uid:draw-rectangle x y *power-width* *power-height* :filledp nil))

(defun directions (dir)
  (case dir
    (:north '(:north :north-east :north-west))
    (:south '(:south :south-east :south-west))
    (:east '(:east :south-east :north-east))
    (:west '(:wost :north-west :south-west))
    (otherwise nil)))

(defun find-direction (dir1 dir2)
  (if (or (null dir1) (null dir2))
    (or dir1 dir2)
    (car (intersection (directions dir1) (directions dir2)))))

(defun case= (a b)
  (and (= (first a) (first b))
       (= (second a) (second b))))

(defun case+ (a b)
  (list (+ (first a) (first b))
        (+ (second a) (second b))))

(defun random-case ()
  (list (random *n-cases-x*)
        (random *n-cases-y*)))

(defun in-board (c)
  "return T if the case C is in the board"
  (and (>= (first c) 0) (>= (second c) 0)
       (< (first c) *n-cases-x*) (< (second c) *n-cases-y*)))

(defun load-image (name)
  (make-instance 'uid:image
                :texture-filepath (merge-pathnames *ressources-dir*
                                                    name)))
(defun draw-at (x y drawable)
  (uid:draw drawable
            :x (+ (* x *tile-width*)
                  (if (oddp y) (/ *tile-width* 2) 0))
            :y (* y *tile-height*)))

(defclass graphic-item ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)))

(defgeneric draw (item)
  (:documentation "Draw an item on the board"))

(defmethod item-position ((item graphic-item))
  (list (x item) (y item)))

(defmethod (setf item-position) (pos (item graphic-item))
  (setf (x item) (first pos))
  (setf (y item) (second pos))
  (list (first pos) (second pos)))

(defmethod pos= ((a graphic-item) (b graphic-item))
  (case= (item-position a) (item-position b)))
