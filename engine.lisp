(in-package robotime)

(eval-when (:compile-toplevel)
  (defvar *keys*                        ; TODO: add qwerty bindings
    '((quit . :escape)
      (left . (:left :a))
      (right . (:right :i))
      (up . (:up :eacute))
      (down . (:down :y))
      (upleft . :b)
      (upright . :p)
      (downleft . :agrave)
      (downright . :x)
      (forward . (:espace :u))
      (backward . :o))
    "The actions with the associated keybindings"))

(defvar *actual-time* 0 "The actual time of the game")

(defclass robotime (uid:simple-game-engine)
  ((board :reader board :initform (make-instance 'board))
   (player :reader player :initform (make-player))
   (entitie :accessor entities :initform nil)  ; TODO: rename as bonus
   (robots :accessor robots :initform nil))
  (:default-initargs :title "Robotime"
    :fps-limit 30
    :width 600
    :height 800))

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
  (run))

(defmethod update ((game robotime))
  "Method called aftear each movement"
  (incf *actual-time*)
  (update-player-collisions (player game) (entities game))
  (delete-entities game)
  (mapcar (lambda (x) (move-robot x (player game))) (robots game))
  (update-player-collisions (player game) (robots game))
  (update-robots-collisions (robots game))
  (let ((new-bonus (spawn-bonus-when-needed (player game) (entities game))))
    (when new-bonus
      (push new-bonus (entities game)))))

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

(defun opposed-direction (dir)
  (case dir
    (:north :south)
    (:south :north)
    (:east :west)
    (:west :east)
    (:north-east :south-west)
    (:north-west :south-east)
    (:south-east :north-west)
    (:south-west :north-east)))

(defmethod correct-position ((player player) robots)
  (and (in-board (item-position player))
       (not (find player robots              ; player can't walk on robot's garbage
                  :test (lambda (p r)
                          (and (not (alivep r))
                               (pos= p r)))))))

(defun player-can-move (player dir robots)
  (move player dir)
  (let ((correct-position (correct-position player robots)))
    (move player (opposed-direction dir))
    correct-position))

(defmethod delete-entities ((game robotime))
  (setf (entities game) (remove-if #'uselessp (entities game))))

(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*))
  (when (null (robots game))
    (setf (robots game)
          (spawn-robots 10 (item-position (player game))))))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game)
  (draw (player game))
  (mapcar #'draw (entities game))
  (mapcar #'draw (robots game))
  (draw-power (- (uid:width game) (* 2 *power-width*)) 10
              (power (player game)) (max-power (player game)))
  (uid:draw (format nil "time: ~a" *actual-time*)
            :x 10 :y (- (uid:width game) 50))
  (draw (board game)))

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
                      (when (player-can-move (player game) ,dir (robots game))
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
  (when (plusp (power (player game)))
    (decf *actual-time*)
    (update-player-collisions (player game) (entities game))
    (update-player-collisions (player game) (robots game))
    (mapcar #'move-backward (robots game))
    (add-power (player game) -3)))