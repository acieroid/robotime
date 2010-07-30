(in-package robotime)

(defparameter *font-size* 10)
(defparameter *power-width* 16 "The width of the power indicator")
(defparameter *power-height* 100 "The maximal height of the power indicator")
(defparameter *power-color* uid:*blue* "The color of the power indicator")
(defparameter *tile-width* 32)
(defparameter *tile-height* (/ *tile-width* 4))
(defparameter *n-cases-x* 20)
(defparameter *n-cases-y* 40)
(defparameter *resources-dir* (merge-pathnames "resources/"
                                               *default-pathname-defaults*))
;; This gets defined here cause we use it everywhere
(defvar *actual-time* 0 "The actual time of the game")

(defun draw-power (x y value max-value)
  (uid:draw-rectangle x y *power-width*
                      (* (/ value max-value) *power-height*)
                      :color *power-color*)
  (uid:draw-rectangle x y *power-width* *power-height* :filledp nil)
  (uid:draw (format nil "power: ~d/~d" value max-value) :x (- x 20) :y (- y 20)))

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

(defun get-direction (dir y)
  (case dir
    (:north '(0 2))
    (:south '(0 -2))
    (:east '(1 0))
    (:west '(-1 0))
    (:north-east (if (evenp y) '(0 1) '(1 1)))
    (:north-west (if (evenp y) '(-1 1) '(0 1)))
    (:south-east (if (evenp y) '(0 -1) '(1 -1)))
    (:south-west (if (evenp y) '(-1 -1) '(0 -1)))
    (otherwise (error "Not a valid direction: ~a" dir))))

(defun opposed-direction (dir)
  (case dir
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)
    (:north-east :south-west)
    (:north-west :south-east)
    (:south-east :north-west)
    (:south-west :north-east)))

(defun cases-around (case)
  "Return the cases around the case CASE. Don't check if cases returned are
in the board"
  (loop for dir in (mapcar (lambda (dir) (get-direction dir (second case)))
                           '(:north :south :east :west
                             :north-east :north-west :south-east :south-west))
       collect (case+ case dir)))

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
                :texture-filepath (merge-pathnames *resources-dir*
                                                    name)))
(defun draw-at (x y drawable)
  (uid:draw drawable
            :x (+ (* (+ x 0) *tile-width*)
                  (if (oddp y) (/ *tile-width* 2) 0))
            :y (* (+ y 2) *tile-height*)))

(defun draw-tile (x y drawable)
  (draw-at (- x 1) (- y 3) drawable))

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
