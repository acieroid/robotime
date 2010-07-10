(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1)
   (positions :accessor positions :initform '())))

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

(defclass player (entity)
  ())

(defun make-player ()
  (make-instance 'player
                  :x (random *n-cases*)
                  :y (random *n-cases*)
                  :time-born *actual-time*))

(defmethod draw ((player player))
  (draw-rectangle-in-case (x player) (y player)
                          (/ *case-size* 2)
                          :color uid:*red*))

(defmethod move ((player player) direction forwardp)
  (declare (ignore forwardp)))