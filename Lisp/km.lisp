;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467

;;; Funzioni principali

(defun km (observations k)
  (if (< (length observations) k)
      (error "Haha get cancer Antoniotti")
      (km-r observations NIL (initialize observations k))))

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

(defun km-r (observations clusters cs)
  (let ((new-clusters (partition observations cs)))
       (if (equal clusters new-clusters) new-clusters
           (km-r observations new-clusters (re-centroids clusters)))))

;;; Crea k centroidi iniziali, ad esempio usando il metodo di Forgy che sceglie
;;; casualmente k delle osservazioni iniziali
(defun initialize (observations k)
  (let ((rand (nth (random (length observations)) observations)))
    (if (equalp k 0) NIL
        (cons rand (initialize (remove rand observations) (- k 1))))))

;;; Raggruppa le observations attorno ai k centroidi in cs
(defun partition (observations cs)
  (partition-r (remove-first (remove-duplicates (sort (partition-n
                                                       observations
                                                       cs)
                                                 #'<
                                                 :key #'car)
                              :key #'third
                              :from-end t)) cs))

(defun partition-r (observations cs)
  (if (null cs) NIL
      (cons (remove-duplicates (partition-a observations (car cs)))
              (partition-r observations (cdr cs)))))

(defun partition-n (observations cs)
  (if (null cs) NIL
      (append (norm-r observations (car cs))
              (partition-n observations (cdr cs)))))

(defun partition-a (observations c)
  (if (null observations) NIL
      (append (cdr (assoc c observations :test #'equal))
              (partition-a (cdr observations) c))))

(defun remove-first (observations)
  (if (null observations) NIL
  (append (list (cdr (car observations)))
          (remove-first (rest observations)))))

;;; Ricalcola il centroide di ogni gruppo
(defun re-centroids (clusters)
  (mapcar #'centroid clusters))

(defun norm-r (observations c)
  (mapcar #'(lambda (v)
                    (list (norm (vsub v c))
                          c
                          v))
                  observations))
