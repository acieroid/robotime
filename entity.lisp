(in-package robotime)

(defvar *lifes-used* 0)

(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1 :initarg :time-died)
   (positions :accessor positions :initform '())
   (useless :accessor uselessp :initform nil)))

(defgeneric collision (entity entity)
  (:documentation "Handle a collision between two entities"))

(defgeneric move (entity direction)
  (:documentation "Move the entity in a direction, this should also
  handle all the consequences of this movement (crash etc.). DIRECTION
  is a keyword indicating a cardinal direction (eg. :NORTH,
  :NORTH-EAST etc.)"))

(defmethod move :after ((entity entity) direction)
  (when direction
    (setf (item-position entity) (case+ (item-position entity)
                                        (get-direction direction
                                                       (y entity))))))

(defmethod alivep ((entity entity))
  (and (<= (time-born entity) *actual-time*)
       (or (= (time-died entity) -1)
           (> (time-died entity) *actual-time*))))

(defmethod kill ((entity entity))
  (setf (time-died entity) *actual-time*))

;;; Player
(defvar *player-tile* (load-image "player.png"))

(defclass player (entity)
  ((power :accessor power :initform 50)
   (max-power :accessor max-power :initform 100)
   (blasts :accessor blasts :initform 0)))

(defun make-player ()
  (destructuring-bind (x y) (random-case)
    (make-instance 'player :x x :y y :time-born *actual-time*)))

(defmethod draw ((player player))
  (draw-at (x player) (y player) *player-tile*))

(defmethod move ((player player) direction)
  (declare (ignore player direction)))

(defmethod add-power ((player player) value)
  (setf (power player)
        (min (max-power player)
             (max 0 (+ (power player) value)))))

(defmethod add-blast ((player player))
  (incf (blasts player)))
