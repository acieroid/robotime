(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1)
   (positions :accessor positions :initform '())
   (color :reader color :initarg :color)))

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
  (and (< (time-born entity) *actual-time*)
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
  ((power :accessor power :initform 0)
   (max-power :accessor max-power :initform 100)
   (color :initform *player-color*)))

(defun make-player ()
  (make-instance 'player
                  :x (random *n-cases*)
                  :y (random *n-cases*)
                  :time-born *actual-time*))

(defmethod move ((player player) direction forwardp)
  (declare (ignore forwardp)))

;;; Bonus
(defclass bonus (entity)
  ((letter :reader letter)
   (color :initform *bonus-color*)))

(defmethod draw :after ((bonus bonus))
  (when (alivep bonus)
    (draw-letter-in-case (x bonus) (y bonus) (letter bonus))))

(defclass power-bonus (bonus)
  ((letter :initform #\p)
   (value :accessor value :initform 10)))

;; TODO: don't just kill it, but completely remove it
(defmethod collision ((player player) (bonus power-bonus))
  (when (alivep bonus)
    (setf (power player) (min (max-power player)
                              (+ (power player) (value bonus))))
    (kill bonus)))

;;; Malus
(defclass malus (entity)
  ((cases :reader cases :initarg :cases)))

(defmethod draw ((malus malus))
  (when (alivep malus)
    (loop for (x y) in (cases malus)
       do (draw-rectangle-in-case x y *case-size* :color *malus-color*))))

(defmethod pos= ((player player) (malus malus))
  (find t  (mapcar (lambda (pos)
                     (and (= (first pos) (x player))
                          (= (second pos) (y player))))
                   (cases malus))))


(defun cases-accross (c)
  "Return the cases accross "
  (loop for dir in '((0 1) (1 0) (1 1) (0 -1) (-1 0) (-1 1) (1 -1))
       for case = (list  (+ (first c) (first dir))
                         (+ (second c) (second dir)))
       when (in-board case)
       collect case))

;; TODO
(defun malus-cases (n case &optional cases)
  "Return n cases that can be used for a malus, around (CENTER-X, CENTER-Y)"
  (when (plusp n)
    (let* ((count 0)
           (new-cases (loop for c in (cases-accross case)
                         when (and (< count n)
                                   (not (find c cases :test #'case=)))
                         collect c
                         do (incf count))))
      (nconc cases new-cases))))

(defclass power-malus (malus)
  ((value :accessor value :initform 5)))

;; TODO: same as for bonus
(defmethod collision ((player player) (malus power-malus))
  (when (alivep malus)
    (setf (power player) (max 0 (- (power player) (value malus))))
    (kill malus)))
