(defparameter *font-size* 14)
(defparameter *keys*
  '((quit . :escape)))

(defclass robotime (uid:simple-game-engine)
  ()
  (:default-initargs :fps-limit 30))

(defvar *engine* (make-instance 'robotime))  ; for debugging purposes only

(defun run ()
  (uid:run *engine*))

(defmethod uid:init ((game robotime))
  (setf uid:*font* (make-instance 'uid::ftgl-font
                                  :filepath #P"font.ttf"
                                  :size *font-size*)))

(defmethod uid:on-draw ((game robotime))
  (uid:clear game))

(defmacro defkey (action &body body)
  `(defmethod uid:on-key-down ((game robotime) keycode
                               (keysym (eql (cdr (assoc ',action *keys*))))
                               string)
     ,@body))

(defkey quit
  (uid:close-window game))