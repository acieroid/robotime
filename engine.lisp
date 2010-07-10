(defparameter *font-size* 14)
(defparameter *keys*                    ; TODO: add qwerty bindings
  '((quit . :escape)
    (left . (:left :a))
    (right . (:right :i))
    (up . (:up :eacute))
    (down . (:down :y))
    (upleft . :b)
    (upright . :p)
    (downleft . :agrave)
    (downright . :x))
  "The actions with the associated keybindings")

(defvar *actual-time* 0 "The actual time of the game")

(defclass robotime (uid:simple-game-engine)
  ((board :reader board :initform (make-instance 'board))
   (player :reader player :initform (make-player)))
  (:default-initargs :title "Robotime"
    :fps-limit 30
    :width 600
    :height 800))

(defparameter *engine* (make-instance 'robotime))  ; for debugging purposes only

(defun run ()
  (handler-bind ((babel-encodings:invalid-utf8-starter-byte
                  (lambda (c) (invoke-restart 'continue))))
    (uid:run *engine*)))

(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*)))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game)
  (draw (board game))
  (draw (player game)))

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

(defkey quit
  (uid:close-window game))

(defmacro defdirections (keys directions)
  (cons 'progn
        (loop for key in keys
           for dir in directions
           collect `(defkey ,key
                      (move (player game) ,dir t)))))

(defdirections
    (up down left right upright upleft downright downleft)
    (:north :south :west :east :north-east :north-west :south-east :south-west))
