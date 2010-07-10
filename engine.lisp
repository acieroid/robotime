(defparameter *keys*                    ; TODO: add qwerty bindings
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
  "The actions with the associated keybindings")

(defvar *actual-time* 0 "The actual time of the game")

(defclass robotime (uid:simple-game-engine)
  ((board :reader board :initform (make-instance 'board))
   (player :reader player :initform (make-player))
   (entities :accessor entities :initform (list
                                           (make-instance 'power-bonus
                                                          :x 10 :y 4)
                                           (make-instance 'power-malus
                                                          :cases '((0 0)
                                                                   (0 1)
                                                                   (1 0)
                                                                   (2 0)
                                                                   (2 1))))))
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

(defmethod update ((game robotime))
  "Method called aftear each movement"
  (incf *actual-time*)
  (update-collisions game))

(defmethod update-collisions ((game robotime))
  (mapcar (alexandria:curry #'collision (player game))
          (remove-if-not
           (alexandria:curry #'pos= (player game))
           (entities game))))

(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*)))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game)
  (draw (player game))
  (mapcar #'draw (entities game))
  (draw-power (- (uid:width game) (* 2 *power-width*)) 10
              (power (player game)) (max-power (player game)))
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
                      (move (player game) ,dir t)
                      (update game)))))

(defkey quit
  (uid:close-window game))

(defdirections
    (up down left right upright upleft downright downleft)
    (:north :south :west :east :north-east :north-west :south-east :south-west))

;; forwarding the time is just a simple wait for the moment
(defkey forward
  (update game)
  (incf (power (player game))))

(defkey backward
  (decf *actual-time*)
  (update-collisions game)
  (decf (power (player game))))