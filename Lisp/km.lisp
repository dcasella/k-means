;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467

;;; Funzioni principali

;;; Il parametro observations è una lista di vettori (ovvero liste)
;;; Il parametro k è il numero di clusters da generare
;;; Il risultato clusters è una lista di gruppi di vettori
(defun km (observations k)
  ;; Controlla se il numero di osservazioni è minore di k
  (if (< (length observations) k)
      ;; Errore: impossibile computare i clusters
      (error "Can't compute clusters.")
      ;; Prosegui con l'algoritmo
      (km-r observations NIL (initialize observations k))))

;;; Ritorna il centroide dell’insieme di osservazioni observations
(defun centroid (observations)
  ;; Dividi ogni coordinata del vettore generato da reduce per il numero
  ;; di osservazioni
  (mapcar #'(lambda (coord) (/ coord (length observations)))
          ;; Somma (facendo uso di vsum) le osservazioni
          (reduce #'vsum observations)))

;;; Calcola la somma (vettoriale) di due vettori
(defun vsum (vector1 vector2)
  (mapcar #'+ vector1 vector2))

;;; Calcola la differenza (vettoriale) di due vettori
(defun vsub (vector1 vector2)
  (mapcar #'- vector1 vector2))

;;; Calcola il prodotto interno di due vettori
(defun innerprod (vector1 vector2)
  (reduce #'+ (mapcar #'* vector1 vector2)))

;;; Calcola la norma euclidea di un vettore
(defun norm (vector)
  (sqrt (innerprod vector vector)))

;;; Funzioni ausiliarie

;;; Crea k centroidi iniziali usando il metodo di Forgy
;;; Metodo di Forgy: sceglie casualmente k delle osservazioni iniziali
(defun initialize (observations k)
  ;; rand = Vettore estratto da observations dato un indice casuale
  (let ((rand (nth (random (length observations)) observations)))
    ;; Caso base: la lista risultante è composta da k vettori
    (if (equalp k 0) NIL
        ;; Rimuovi il vettore selezionato da observations
        ;; per non incorrerci nuovamente nelle ricorsioni future
        (cons rand (initialize (remove rand observations) (- k 1))))))

;;; Ritorna la lista di gruppi (di liste) di vettori (anch'essi liste)
;;; raggrupparti per centroide
(defun km-r (observations clusters cs)
  ;; new-clusters = Lista di gruppi di vettori ottenuta raggruppando
  ;; le observations attorno ai centroidi in cs
  (let ((new-clusters (partition observations cs)))
       ;; Caso base: i clusters calcolati nella ricorsione attuale sono
       ;; uguali a quelli calcolati nella ricorsione precedente
       (if (equal clusters new-clusters) clusters
           ;; Computa ricorsivamente i clusters con nuovi centroidi
           (km-r observations new-clusters (re-centroids new-clusters)))))

;;; Raggruppa le observations attorno ai k centroidi in cs
(defun partition (observations cs)
  ;; Calcola la lista di liste di tris (Distanza Centroide Vettore)
  ;; Ordina il risultato di partition-n secondo la Distanza
  ;; Rimuovi i vettori (#'third) duplicati a partire dal fondo
  ;; Rimuovi la Distanza dai tris (diventando così coppie)
  ;; Raggruppa le coppie (Centroide Vettore) in liste di vettori (clusters)
  (partition-r (remove-first (remove-duplicates (sort (partition-n
                                                       observations
                                                       cs)
                                                 #'<
                                                 :key #'car)
                              :key #'third
                              :from-end t)) cs))

;;; Ritorna la lista di liste di tris
;;; Tris: (Distanza Centroide Vettore)
(defun partition-n (observations cs)
  ;; Caso base: non ci sono centroidi da computare
  (if (null cs) NIL
      ;; Calcola la lista di Tris per il primo centroide e ricorsivamente
      ;; per per ogni centroide
      (append (norm-r observations (car cs))
              (partition-n observations (cdr cs)))))

;;; Calcola la distanza tra ogni vettore di observations ed il vettore c
;;; e ritorna una lista di tris (Distanza Centroide Vettore)
(defun norm-r (observations c)
  (mapcar #'(lambda (v)
                    (list (norm (vsub v c)) ; Sottrai i due vettori e calcola
                          c                 ; la norma; crea una lista (tris)
                          v))               ; di Distanza, Centroide, Vettore
                  observations))

;;; Rimuovi il primo elemento di ogni sotto-lista di observations
(defun remove-first (observations)
  ;; Caso base: non ci sono liste da computare
  (if (null observations) NIL
      ;; Estrai il resto della prima sotto-lista appartenente a observations
      ;; e ricorsivamente da ogni sotto-lista
      (cons (cdr (car observations))
            (remove-first (cdr observations)))))

;;; Ritorna la lista di liste di vettori raggruppati per centroide
(defun partition-r (observations cs)
  ;; Caso base: non ci sono centroidi da computare
  (if (null cs) NIL
      ;; Calcola la lista di vettori per il primo centroide (rimuovendo gli
      ;; eventuali duplicati) e ricorsivamente per ogni centroide
      (cons (remove-duplicates (partition-a observations (car cs)))
              (partition-r observations (cdr cs)))))

;;; Ritorna la lista di vettori appartenenti alla coppia (Vettore Centroide)
(defun partition-a (observations c)
  ;; Caso base: non ci sono coppie da computare
  (if (null observations) NIL
      ;; Estrai il primo vettore avente come centroide corrispondente c
      ;; e ricorsivamente dal resto di observations (cdr ...)
      (append (cdr (assoc c observations :test #'equal))
              (partition-a (cdr observations) c))))

;;; Ricalcola il centroide di ogni gruppo
(defun re-centroids (clusters)
  (mapcar #'centroid clusters))
