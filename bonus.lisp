(in-package robotime)

(defparameter *bonus-frequency* 15)
(defparameter *frequencies*
  '(power-bonus power-bonus power-bonus
    ))
(defparameter *frequencies-length* (length *frequencies*))
(defvar *bonus-tile* (load-image "bonus.png"))

(defvar *last-spawn* 0)

(defclass bonus (entity)
  ((letter :reader letter)
   (duration :reader duration :initform 10)))

(defmethod draw ((bonus bonus))
  (when (alivep bonus)
    (draw-at (x bonus) (y bonus) *bonus-tile*)))

(defmethod collision :after ((player player) (bonus bonus))
  (setf (uselessp bonus) t))

(defclass power-bonus (bonus)
  ((letter :initform #\p)
   (value :accessor value :initform 10)))

(defmethod collision ((player player) (bonus power-bonus))
  (when (alivep bonus)
    (add-power player (value bonus))))

;; Spawn related stuff
;; TODO: there are a lot of improvements to do here
(defun random-bonus (case)
  (make-instance (nth (random (1- *frequencies-length*)) *frequencies*)
                 :x (first case) :y (second case)
                 :time-born *actual-time*
                 :time-died (+ *actual-time* 20))) ; TODO: add some randomness

(defun spawn-needed ()
  (and (> *actual-time* *last-spawn*)   ; don't spawn if we're in the past
       (<= (+ *last-spawn* *bonus-frequency*)
          *actual-time*)))               ; TODO: add some randomness here too

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
