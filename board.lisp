(in-package robotime)

(defvar *grass-tile* (make-instance 'uid:image
                                    :texture-filepath #p"/home/quentin/grass.png"))
(defclass board (graphic-item)
  ())

(defmethod draw ((board board))
  (loop for y from *n-cases-y* downto 0
       do (loop for x from 0 to *n-cases-x*
             do (draw-at x y *grass-tile*))))
