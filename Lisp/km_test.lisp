(defun make-point (dimensions limit)
  (loop repeat dimensions collect
    (- (random (* limit 2)) limit)))

(defun make-space (dimensions limit points)
  (loop repeat points collect
    (make-point dimensions limit)))
