(in-package robotime)

(defparameter *difficulty-by-level* 5)
(defvar *level* 1)

(eval-when (:compile-toplevel)
  (defparameter *layout* 'bepo)
  (defparameter *keys*
    `((quit . :escape) (blast . :space)
      ,@(case *layout*
             (bepo '((left . (:left :a)) (right . (:right :i))
                       (up . (:up :eacute)) (down . (:down :y))
                       (upleft . :b) (upright . :p) (downleft . :agrave)
                       (downright . :x) (forward . :u) (backward . :o)))
             (qwerty '((left . (:left :d)) (right . (:right :g))
                       (up . (:up :r)) (down . (:down :v))
                       (upleft . :e) (upright . :t) (downleft . :c)
                       (downright . :b) (forward . :f) (backward . :y)))))
    "The actions with the associated keybindings"))

(defclass robotime (uid:simple-game-engine)
  ((board :reader board :initform (make-instance 'board))
   (player :reader player :initform (make-player))
   (entities :accessor entities :initform nil)
   (robots :accessor robots :initform nil))
  (:default-initargs :title "Robotime"
    :fps-limit 30
    :width 800
    :height 600))

(defparameter *engine* (make-instance 'robotime))  ; for debugging purposes only

(defun run ()
  "Launch the game"
  (flet ((dont-hang (c)
           (declare (ignore c))
           (invoke-restart 'continue)))
    ;; This avoids encoding and weird opengl issues, not the best solution though
    (handler-bind ((babel-encodings:invalid-utf8-starter-byte #'dont-hang)
                   (cl-opengl-bindings:opengl-error #'dont-hang))
      (uid:run *engine*))))

(defun reinit ()
  (setf *actual-time* 0
        *last-spawn* 0)
  (setf *engine* (make-instance 'robotime))
  (setf *level* 1)
  (run))

(defmethod update ((game robotime))
  "Method called aftear each movement"
  (incf *actual-time*)
  (update-player-collisions (player game) (entities game))
  (delete-entities game)
  (mapcar (lambda (x) (move-robot x (player game))) (robots game))
  (update-player-collisions (player game) (robots game))
  (update-robots-collisions (robots game))
  (delete-robots game)
  (let ((new-bonus (spawn-bonus-when-needed (player game) (entities game))))
    (when new-bonus
      (push new-bonus (entities game))))
  (when (null (remove-if-not #'alivep (robots game)))
    (levelup game)))

(defun update-player-collisions (player entities)
  (mapcar (lambda (x)
            (when (pos= x player)
              (collision player x)))
          entities))

(defun update-robots-collisions (robots)
  (when robots
    (let ((robot (first robots))
          (rest (rest robots)))
      (mapcar (lambda (x)
                (when (pos= x robot)
                  (collision x robot)))
              rest)
      (update-robots-collisions rest))))

(defun player-can-move (player dir)
  (move player dir)
  (let ((correct-position (in-board (item-position player))))
    (move player (opposed-direction dir))
    correct-position))

(defmethod levelup ((game robotime))
  (incf *level*)
  (setf *actual-time* 0
        *last-spawn* 0)
  (setf (entities game) nil)
  (setf (robots game) (spawn-robots (* *difficulty-by-level* *level*)
                                    (item-position (player game)))))

(defmethod delete-entities ((game robotime))
  (setf (entities game) (remove-if #'uselessp (entities game))))

(defmethod delete-robots ((game robotime))
  (setf (robots game) (remove-if #'uselessp (robots game))))

(defun blast (center robots)
  (let ((cases (cases-around center)))
    (mapcar
     (lambda (robot)
       (when (find (item-position robot) cases
                   :test #'case=)
         (kill robot)))
     robots)))

(defmacro draw-informations (x y step &rest infos)
  (cons 'progn
        (loop for info in infos
           for offset from step by step
           collect
           (destructuring-bind (control-string &rest args) info
             `(uid:draw (format nil ,control-string ,@args)
                        :x ,x :y (- ,y ,offset))))))

(defmethod draw-ui ((game robotime))
  (draw-power (- (uid:width game) (* 2 *power-width*)) 40
              (power (player game)) (max-power (player game)))
  (draw-informations 10 (- (uid:height game) 50) 10
    ("time: ~a" *actual-time*)
    ("robots: ~a" (loop for robot in (robots game) count (alivep robot)))
    ("level: ~a" *level*)
    ("blasts: ~a" (blasts (player game)))
    ("lifes used: ~a" *lifes-used*)))

;;; UID functions
(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*))
  (when (null (robots game))
    (setf (robots game)
          (spawn-robots *difficulty-by-level* (item-position (player game))))))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game)
  (draw (board game))
  (mapcar #'draw (entities game))
  (mapcar #'draw (robots game))
  (draw (player game))
  (draw-ui game))

;; Keybindings
(defmacro defkey (action &body body)
  (let* ((keys (cdr (assoc action *keys*)))
         (keysyms
          (etypecase keys
            (keyword (cons keys nil))
            (list keys))))
    (cons 'progn
          (loop for keysym in keysyms
             collect
             `(defmethod uid:on-key-down ((game robotime) keycode
                                          (keysym (eql ,keysym))
                                          string)
                ,@body)))))

(defmacro defdirections (keys directions)
  (cons 'progn
        (loop for key in keys
           for dir in directions
           collect `(defkey ,key
                      (when (player-can-move (player game) ,dir)
                        (move (player game) ,dir)
                        (update game))))))

(defkey quit
  (uid:close-window game))

(defdirections
    (up down left right upright upleft downright downleft)
    (:north :south :west :east :north-east :north-west :south-east :south-west))

;; forwarding the time is just a simple wait for the moment
(defkey forward
  (update game)
  (add-power (player game) 3))

(defkey backward
  (when (and (>= (power (player game)) 3)
             (plusp *actual-time*))
    (decf *actual-time*)
    (update-player-collisions (player game) (entities game))
    (update-player-collisions (player game) (robots game))
    (mapcar #'move-backward (robots game))
    (add-power (player game) -3)))

(defkey blast
  (when (plusp (blasts (player game)))
    (decf (blasts (player game)))
    (blast (item-position (player game)) (robots game))))
