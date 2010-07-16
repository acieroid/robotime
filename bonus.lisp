(in-package robotime)

(defparameter *bonus-min-frequency* 5)
(defparameter *bonus-max-frequency* 25)
(defparameter *bonus-min-duration* 20)
(defparameter *bonus-max-duration* 40)
(defvar *last-spawn* 0)
(defvar *bonus* nil)
(defvar *n-bonus* 0)

(defclass bonus (entity)
  ((duration :reader duration :initform 10)))

(defmethod collision :after ((player player) (bonus bonus))
  (setf (uselessp bonus) t))

(defmacro new-bonus (name file &body action)
  (let ((tile (gensym)))
    `(progn
       (let ((,tile (load-image ,file)))
         (defclass ,name (bonus) ())
         (defmethod draw ((bonus ,name))
           (when (alivep bonus)
             (draw-at (x bonus) (y bonus) ,tile)))
         (defmethod collision ((player player) (bonus ,name))
           (when (alivep bonus)
             ,@action))
         (push ',name *bonus*)
         (incf *n-bonus*)))))

(new-bonus power "bonus.png"
  (add-power player 10))
(new-bonus blast "bonus2.png"
  (add-blast player))

;; Spawn related stuff
(defun random-bonus (case)
  (make-instance (nth (random *n-bonus*) *bonus*)
                 :x (first case) :y (second case)
                 :time-born *actual-time*
                 :time-died (+ *actual-time* *bonus-min-duration*
                               (random
                                (- *bonus-max-duration* *bonus-min-duration*)))))

(defun spawn-needed ()
  (and (> *actual-time* *last-spawn*)   ; don't spawn if we're in the past
       (<= (+ *last-spawn* *bonus-min-frequency*
              (random (- *bonus-max-frequency* *bonus-min-frequency*)))
          *actual-time*)))

(defun find-free-case (player entities)
  (let ((cases (cons (item-position player) (mapcar #'item-position entities))))
    (loop for case = (random-case)
       when (not (find case cases :test #'case=))
       return case)))

(defun spawn-bonus-when-needed (player entities)
  (when (spawn-needed)
    (let ((bonus (random-bonus (find-free-case player entities))))
      (setf *last-spawn* *actual-time*)
      bonus)))
