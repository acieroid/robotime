(in-package robotime)

(defvar *grass-tile* (load-image "grass.png"))

(defclass board (graphic-item)
 ())

(defmethod draw ((board board))
  (loop for y from *n-cases-y* downto 1
       do (loop for x from 1 to *n-cases-x*
             do (draw-tile x y *grass-tile*))))
