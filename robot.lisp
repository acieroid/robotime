(in-package robotime)

(defclass robot (entity)
  ((color :initform *robot-color*)
   (positions :initform nil)))

(defun make-robot (case)
  (destructuring-bind (x y) case
    (make-instance 'robot :x x :y y
                   :time-born *actual-time*)))

(defun spawn-robots (n player-case &optional (cases nil))
  (if (plusp n)
      (let ((case (list (random *n-cases*)
                        (random *n-cases*))))
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

(defmethod move-robot ((robot robot) (player player))
  (when (alivep robot)
    (let ((direction
           (list 
            (compare (x robot) (x player) 
                     ((< 1)
                      (> -1)
                      (= 0)))
            (compare (y robot) (y player)
                     ((< 1)
                      (> -1)
                      (= 0))))))
      (push (item-position robot) (positions robot))
      (setf (item-position robot) (case+ (item-position robot) direction)))))

(defmethod backward ((robot robot))
  (when (alivep robot)
    (setf (item-position robot) (pop (positions robot)))))

(defmethod draw :after ((robot robot))
  (when (not (alivep robot))
    (draw-rectangle-in-case (x robot) (y robot) *entity-size*
                            :color (color robot))
    (draw-rectangle-in-case (x robot) (y robot) (/ *entity-size* 2)
                            :color uid:*blue*)))

;; TODO: restart the level or finish the game
(defmethod collision ((player player) (robot robot))
  (setf (item-position player) (list (random *n-cases*)
                                     (random *n-cases*))))

(defmethod collision ((a robot) (b robot))
  (kill a)
  (kill b))