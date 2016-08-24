;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467

;;; Funzioni principali

(defun km (observations k)
  ())

(defun centroid (observations)
  (mapcar #'(lambda (coord) (/ coord (length observations)))
          (reduce #'vsum observations)))

(defun vsum (vector1 vector2)
  (mapcar #'+ vector1 vector2))

(defun vsub (vector1 vector2)
  (mapcar #'- vector1 vector2))

(defun innerprod (vector1 vector2)
  (reduce #'+ (mapcar #'* vector1 vector2)))

(defun norm (vector)
  (sqrt (innerprod vector vector)))

;;; Funzioni ausiliarie

