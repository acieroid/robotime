(in-package robotime)

(defparameter *bonus-frequency* 15)
(defparameter *frequencies*
  '(power-bonus power-bonus power-bonus
    ;power-malus
    ))
(defparameter *frequencies-length* (length *frequencies*))
(defvar *bonus-tile* (load-image "bonus.png"))

(defvar *last-spawn* 0)

;;; Bonus
(defclass bonus (entity)
  ((letter :reader letter)
   (color :initform *bonus-color*)
   (duration :reader duration :initform 10)))

(defmethod draw ((bonus bonus))
  (when (alivep bonus)
    (draw-rectangle-in-case (x bonus) (y bonus) (1- *case-size*)
                            :color (color bonus))
    (draw-letter-in-case (x bonus) (y bonus) (letter bonus))))

(defmethod collision :after ((player player) (bonus bonus))
  (setf (uselessp bonus) t))

(defclass power-bonus (bonus)
  ((letter :initform #\p)
   (value :accessor value :initform 10)))

(defmethod collision ((player player) (bonus power-bonus))
  (when (alivep bonus)
    (add-power player (value bonus))))

(defmethod draw ((bonus bonus))
  (draw-at (x bonus) (y bonus) *bonus-tile*))

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
  (find t (mapcar (lambda (pos)
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

(defmethod collision ((player player) (malus power-malus))
  (when (alivep malus)
    (add-power player (- (value malus)))))

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
  (let ((cases (reduce (lambda (last x)
                         (append last (cases-occupied x)))
                       (cons nil
                             (cons player entities)))))
    (loop for case = (list (random *n-cases*)
                           (random *n-cases*))
       when (not (find case cases :test #'case=))
       return case)))

;; TODO: spawn malus too
(defun spawn-bonus-when-needed (player entities)
  (when (spawn-needed)
    (let ((bonus (random-bonus (find-free-case player entities))))
      (setf *last-spawn* *actual-time*)
      bonus)))

(defmethod cases-occupied ((item graphic-item))
  (list (list (x item) (y item))))

(defmethod cases-occupied ((malus malus))
  (cases malus))