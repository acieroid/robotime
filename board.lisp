(defclass board (graphic-item)
  ((size :reader size :initform (* *n-cases* *case-size*))))

(defmethod draw ((board board))
  (loop for x from 1 to (size board) by *case-size*
       do (loop for y from 1 to (size board) by *case-size*
               do (uid:draw-rectangle x y *case-size* *case-size*
                                      :filledp nil))))