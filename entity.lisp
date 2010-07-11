(in-package :robotime)

(defclass entity (graphic-item)
  ((time-born :reader time-born :initarg :time-born)
   (time-died :accessor time-died :initform -1)
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

(defmethod add-power ((player player) value)
  (setf (power player)
        (min (max-power player)
             (max 0 (+ (power player) value)))))

;;; Bonus
(defclass bonus (entity)
  ((letter :reader letter)
   (color :initform *bonus-color*)))

(defmethod draw :after ((bonus bonus))
  (when (alivep bonus)
    (draw-letter-in-case (x bonus) (y bonus) (letter bonus))))

(defmethod collision :after ((player player) (bonus bonus))
  (setf (uselessp bonus) t))

(defclass power-bonus (bonus)
  ((letter :initform #\p)
   (value :accessor value :initform 10)))

;; TODO: don't just kill it, but completely remove it
(defmethod collision ((player player) (bonus power-bonus))
  (when (alivep bonus)
    (add-power player (value bonus))))

;;; Malus
(defclass malus (bonus)
  ((color :initform *malus-color*)
   (cases :reader cases :initarg :cases)))

(defmethod draw ((malus malus))
  (when (alivep malus)
    (loop for (x y) in (cases malus)
       do (progn
            (draw-rectangle-in-case x y *case-size* :color (color malus))
            (draw-letter-in-case x y (letter malus))))))

(defmethod pos= ((player player) (malus malus))
  (find t  (mapcar (lambda (pos)
                     (and (= (first pos) (x player))
                          (= (second pos) (y player))))
                   (cases malus))))

(defun cases-accross (c)
  "Return the cases accross "
  (loop for dir in '((0 1) (1 0) (1 1) (0 -1) (-1 0) (-1 1) (1 -1) (-1 -1))
       for case = (list  (+ (first c) (first dir))
                         (+ (second c) (second dir)))
       when (in-board case)
       collect case))

(defun take (n l)
  (when (and l (plusp n))
    (cons (first l) (take (1- n) (rest l)))))

;; TODO
(defun malus-cases (n case)
  "Return n cases that can be used for a malus, around (CENTER-X, CENTER-Y)"
  (labels ((next-cases (c cases)
             (remove-if (lambda (x)
                          (find x cases :test #'case=))
                        (cases-accross c)))
           (rec (n cases)
             (let ((count 0)
                   (new-cases cases))
               (loop for c in cases
                  while (< count n)
                  do (let* ((cs (next-cases c new-cases))
                            (length (length cs)))
                       (if (> length (- n count))
                           (progn
                             (nconc new-cases (take (- n count) cs))
                             (setf count n))
                           (progn
                             (nconc new-cases cs)
                             (incf count length)))))
               (if (< count n)
                   (rec (- n count) new-cases)
                   new-cases))))
    (rec (1- n) (list case))))

(defclass power-malus (malus)
  ((letter :initform #\p)
   (value :accessor value :initform 5)))

;; TODO: same as for bonus
(defmethod collision ((player player) (malus power-malus))
  (when (alivep malus)
    (add-power player (- (value malus)))))
