(in-package robotime)
(defvar *robot-tile* (load-image "robot.png"))
(defvar *dead-robot-tile* (load-image "dead-robot.png"))

(defclass robot (entity)
  ((positions :initform nil)))

(defun make-robot (case)
  (destructuring-bind (x y) case
    (make-instance 'robot :x x :y y
                   :time-born *actual-time*)))

(defun spawn-robots (n player-case &optional (cases nil))
  (if (plusp n)
      (let ((case (random-case)))
        (if (or (case= player-case case)
                (find case cases :test #'case=))
            (spawn-robots n player-case cases)
            (spawn-robots (1- n) player-case (cons case cases))))
      (mapcar #'make-robot cases)))

(defmacro compare (x y clauses)
  (let ((x-sym (gensym))
        (y-sym (gensym)))
    `(let ((,x-sym ,x)
           (,y-sym ,y))
       (cond
         ,@(mapcar (lambda (clause)
                    `((,(first clause) ,x-sym ,y-sym)
                      ,@(rest clause)))
                  clauses)))))

;; TODO: could be better
(defmethod move-robot ((robot robot) (player player))
  (when (alivep robot)
    (let ((direction
           (find-direction
            (compare (x robot) (x player)
                     ((< :east) (> :west) (= nil)))
            (compare (y robot) (y player)
                     ((< :north) (> :south) (= nil))))))
      (push (item-position robot) (positions robot))
      (move robot direction))))

(defmethod move ((robot robot) direction)
  (declare (ignore robot direction)))

(defmethod move-backward ((robot robot))
  (when (> (time-died robot) *actual-time*)
    (setf (time-died robot) -1))
  (when (alivep robot)
    (setf (item-position robot) (pop (positions robot)))))

(defmethod draw ((robot robot))
  (draw-at (x robot) (y robot) (if (alivep robot)
                                   *robot-tile*
                                   *dead-robot-tile*)))

(defmethod collision ((player player) (robot robot))
  (when (alivep robot)
    (setf (item-position player) (random-case))))

(defmethod collision ((a robot) (b robot))
  (when (alivep a)
    (kill a))
  (when (alivep b)
    (kill b)))