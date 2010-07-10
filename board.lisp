(defparameter *case-size* 16 "The size of a case of the board")
(defparameter *n-cases* 32 "The number of cases on the board")

(defclass board ()
  ((size :reader size :initform (* *n-cases* *case-size*))))

(defmethod draw ((board board))
  (loop for x from 1 to (size board) by *case-size*
       do (loop for y from 1 to (size board) by *case-size*
               do (uid:draw-rectangle x y *case-size* *case-size*
                                      :filledp nil))))