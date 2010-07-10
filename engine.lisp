(defparameter *font-size* 14)
(defparameter *keys*
  '((quit . :escape))
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
  (uid:run *engine*))

(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*)))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game)
  (draw (board game))
  (draw (player game)))

(defmacro defkey (action &body body)
  `(defmethod uid:on-key-down ((game robotime) keycode
                               (keysym (eql (cdr (assoc ',action *keys*))))
                               string)
     ,@body))

(defkey quit
  (uid:close-window game))