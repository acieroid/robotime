(in-package robotime)

(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1 :initarg :time-died)
   (positions :accessor positions :initform '())
   (color :reader color :initarg :color)
   (useless :accessor uselessp :initform nil)))

(defgeneric collision (entity entity)
  (:documentation "Handle a collision between two entities"))

(defgeneric move (entity direction forwardp)
  (:documentation "Move the entity in a direction, this should also
  handle all the consequences of this movement (crash etc.). FORWARDP
  is T if the time is moving forward, NIL if moving backward. DIRECTION
  is a keyword indicating a cardinal direction (eg. :NORTH,
  :NORTH-EAST etc.)"))

(defmethod move :after ((entity entity) direction forwardp)
  (destructuring-bind (x y)
      (case direction
        (:north '(0 1))
        (:south '(0 -1))
        (:east '(1 0))
        (:west '(-1 0))
        (:north-east '(1 1))
        (:north-west '(-1 1))
        (:south-east '(1 -1))
        (:south-west '(-1 -1))
        (otherwise (error "Not a valid direction: ~a" direction)))
    (setf (x entity) (+ (x entity) x))
    (setf (y entity) (+ (y entity) y))))

(defmethod alivep ((entity entity))
  (and (<= (time-born entity) *actual-time*)
       (or (= (time-died entity) -1)
           (> (time-died entity) *actual-time*))))

(defmethod kill ((entity entity))
  (setf (time-died entity) *actual-time*))

;; This only draw when the entity is alive
(defmethod draw ((entity entity))
  (when (alivep entity)
    (draw-rectangle-in-case (x entity) (y entity) *entity-size*
                            :color (color entity))))
;;; Player
(defclass player (entity)
  ((power :accessor power :initform 50)
   (max-power :accessor max-power :initform 100)
   (color :initform *player-color*)))

(defun make-player ()
  (make-instance 'player
                  :x (random *n-cases*)
                  :y (random *n-cases*)
                  :time-born *actual-time*))

(defmethod move ((player player) direction forwardp)
  (declare (ignore forwardp)))

(defmethod add-power ((player player) value)
  (setf (power player)
        (min (max-power player)
             (max 0 (+ (power player) value)))))

