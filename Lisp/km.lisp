;;;; -*- Mode: Lisp -*-

;;;; km.lisp --
;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467



(defun vsum (vector1 vector2)
  "Parameter vector1, vector (list of coordinates).
   Parameter vector2, vector (list of coordinates).
   Calculate the vector sum of two vectors."
  (mapcar #'+ vector1 vector2))

(defun vsub (vector1 vector2)
  "Parameter vector1, vector (list of coordinates).
   Parameter vector2, vector (list of coordinates).
   Calculate the vector difference of two vectors."
  (mapcar #'- vector1 vector2))

(defun centroid (observations)
  "Parameter observations, list of vectors (or lists).
   Calculate the centroid of the set of observations."
  (if (null observations)
    nil
    (mapcar #'(lambda (coord) (/ coord (length observations)))
            (reduce #'vsum observations))))

(defun innerprod (vector1 vector2)
  "Parameter vector1, vector (list of coordinates).
   Parameter vector2, vector (list of coordinates).
   Calculate the dot product (or inner product) of two vectors."
  (reduce #'+ (mapcar #'* vector1 vector2)))

(defun norm (vector)
  "Parameter vector, vector (list of coordinates).
   Calculate the Euclidean norm of a vector."
  (sqrt (innerprod vector vector)))

(defun initialize (observations k)
  "Parameter observations, list of vectors (or lists).
   Parameter k, number of clusters to generate.
   Create k starting centroids using Forgy's method.
   Forgy's method: randomly select k of the starting observations."
  (if (= k 0)
    nil
    (let ((rand (nth (random (length observations)) observations)))
      (cons rand (initialize (remove rand observations) (- k 1))))))

(defun map-cluster (clusters-map observations cl index)
  "Parameter clusters-map, list of indices.
   Parameter observations, list of vectors (or lists).
   Parameter cl, cluster's index.
   Parameter index, index of (car clusters-map) concerning clusters.
   Return the list of vectors representing the cluster of index cl."
  (cond ((null clusters-map) nil)
        ((not (= cl (car clusters-map)))
         (map-cluster (cdr clusters-map) observations cl (+ index 1)))
        (t (cons (nth index observations)
                 (map-cluster (cdr clusters-map) observations cl (+ index 1))))))

(defun map-clusters (clusters-map observations cl k)
  "Parameter clusters-map, list of indices.
   Parameter observations, list of vectors (or lists).
   Parameter cl, index of (car clusters) concerning clusters.
   Parameter k, number of clusters to generate.
   Returns the list of clusters starting from a list of clusters's indices."
  (if (= cl k)
    nil
    (cons (map-cluster clusters-map observations cl 0)
          (map-clusters clusters-map observations (+ cl 1) k))))

(defun re-centroids (clusters-map observations k)
  "Parameter clusters, list of indices.
   Parameter observations, list of vectors (or lists).
   Parameter cl, cluster's index.
   Parameter k, number of clusters to generate.
   Re-calculate every cluster's centroid."
  (mapcar #'centroid (map-clusters clusters-map observations 0 k)))

(defun pick-centroid (v cs old-distance result)
  "Parameter v, vector.
   Parameter cs, list of centroids.
   Parameter old-distance, last distance calculated during recursion.
   Parameter result, a centroid, possible function's result.
   Returns the centroid whose distance between v and itself is
   the lowest calculated."
  (cond ((null cs) result)
        ((null (car cs)) (pick-centroid v (cdr cs) old-distance result))
        (t (let* ((c (car cs)) (new-distance (norm (vsub v c))))
             (cond ((< new-distance old-distance)
                    (pick-centroid v (cdr cs) new-distance c))
                   (t (pick-centroid v (cdr cs) old-distance result)))))))

(defun partition (observations cs)
  "Parameter observations, list of vectors (or lists).
   Parameter cs, list of centroids.
   Returns the Clusters-Map, or a list of clusters's indices,
   given cs as centroids."
  (if (null observations)
    nil
    (cons (position (pick-centroid (car observations)
                                   cs
                                   most-positive-fixnum
                                   cs) cs)
          (partition (cdr observations) cs))))

(defun lloyd-km (observations clusters cs k)
  "Parameter observations, list of vectors (or lists).
   Parameter clusters, list of vectors' groups calculated during the previous
   recursion (nil during the first function call).
   Parameter cs, list of centroids.
   Parameter k, number of clusters to generate.
   Returns the Clusters-Map, or a list of cluster's indices."
  (let ((new-clusters (partition observations cs)))
    (if (equal clusters new-clusters)
      clusters
      (lloyd-km observations
                new-clusters
                (re-centroids new-clusters observations k)
                k))))

(defun km (observations k)
"Parameter observations, list of vectors (or lists).
Parameter k, number of clusters to generate.
Returns k clusters from the set of observations."
  (cond ((< (length observations) k) (error "Can't compute clusters."))
        ((= (length observations) k) observations)
        ((null observations) nil)
        (t (map-clusters (lloyd-km observations
                                   nil
                                   (initialize observations k)
                                   k)
                         observations 0 k))))

;;;; end of file -- km.lisp --
