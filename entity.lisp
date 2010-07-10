(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1)
   (positions :accessor positions :initform '())))

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