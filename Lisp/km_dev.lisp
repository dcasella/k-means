;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467

;;; Funzioni principali

(defun km (observations k)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro k, numero di clusters da generare.
 Ritorna k clusters dell'insieme di osservazioni observations."
  ;; Controlla se il numero di osservazioni è minore di k
  (cond ((< (length observations) k)
          ;; Errore: impossibile computare i clusters
          (error "Can't compute clusters."))
        ;; Controlla se il numero di osservazioni è uguale a k
        ((= (length observations) k)
          ;; Ritorna observations
          observations)
        ;; Controlla se non è possibile computare observations
        ((null observations)
          NIL)
        ;; Prosegui con l'algoritmo
        (T (km-r observations NIL (initialize observations k)))))

(defun centroid (observations)
"Parametro observations, lista di vettori (ovvero liste).
 Calcola il centroide dell'insieme di osservazioni observations."
  ;; Dividi ogni coordinata del vettore generato da reduce per il numero
  ;; di osservazioni
  (mapcar #'(lambda (coord) (/ coord (length observations)))
          ;; Somma (facendo uso di vsum) le osservazioni
          (reduce #'vsum observations)))

(defun vsum (vector1 vector2)
"Parametro vector1, vettore (lista di coordinate).
 Parametro vector2, vettore (lista di coordinate).
 Calcola la somma (vettoriale) di due vettori."
  (mapcar #'+ vector1 vector2))

(defun vsub (vector1 vector2)
"Parametro vector1, vettore (lista di coordinate).
 Parametro vector2, vettore (lista di coordinate).
 Calcola la differenza (vettoriale) di due vettori."
  (mapcar #'- vector1 vector2))

(defun innerprod (vector1 vector2)
"Parametro vector1, vettore (lista di coordinate).
 Parametro vector2, vettore (lista di coordinate).
 Calcola il prodotto interno di due vettori."
  (reduce #'+ (mapcar #'* vector1 vector2)))

(defun norm (vector)
"Parametro vector, vettore (lista di coordinate).
 Calcola la norma euclidea di un vettore."
  (sqrt (innerprod vector vector)))

;;; Funzioni ausiliarie

(defun initialize (observations k)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro k, numero di clusters da generare.
 Crea k centroidi iniziali usando il metodo di Forgy.
 Metodo di Forgy: sceglie casualmente k delle osservazioni iniziali."
  ;; Caso base: la lista risultante è composta da k vettori
  (if (= k 0) NIL
      ;; rand = Vettore estratto da observations dato un indice casuale
      (let ((rand (nth (random (length observations)) observations)))
            ;; Rimuovi il vettore selezionato da observations
            ;; per non incorrerci nuovamente nelle ricorsioni future
            (cons rand (initialize (remove rand observations) (- k 1))))))

(defun km-r (observations clusters cs)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro clusters, lista di gruppi di vettori calcolati nella ricorsione
 precedente (NIL durante la prima chiamata).
 Parametro cs, lista di centroidi.
 Ritorna la lista di gruppi (di liste) di vettori (anch'essi liste)
 raggrupparti per centroide."
  ;; new-clusters = Lista di gruppi di vettori ottenuta raggruppando
  ;; le observations attorno ai centroidi in cs
  (let ((new-clusters (partition observations cs)))
       ;; Caso base: i clusters calcolati nella ricorsione attuale sono
       ;; uguali a quelli calcolati nella ricorsione precedente
       (if (equal clusters new-clusters) clusters
           ;; Computa ricorsivamente i clusters con nuovi centroidi
           (km-r observations new-clusters (re-centroids new-clusters)))))

(defun partition (observations cs)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro cs, lista di centroidi.
 Raggruppa le observations attorno ai k centroidi in cs."
  ;; Calcola la lista di liste di tris (Distanza Centroide Vettore)
  ;; Ordina il risultato di partition-n secondo la Distanza
  ;; Rimuovi i vettori (#'third) duplicati a partire dal fondo
  ;; Rimuovi la Distanza dai tris (diventando così coppie)
  ;; Raggruppa le coppie (Centroide Vettore) in liste di vettori (clusters)
  (partition-r (sort (partition-n observations
                                  cs)
                     #'list<)
               cs))

(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (caar a) (caar b)) (list< (cdr a) (cdr b)))
        (t (< (caar a) (caar b)))))

(defun partition-n (observations cs)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro cs, lista di centroidi.
 Ritorna la lista di liste di tris (Distanza Centroide Vettore)."
  ;; Caso base: non ci sono centroidi da computare
  (if (null observations) NIL
      ;; Calcola la lista di tris per il primo centroide e ricorsivamente
      ;; per per ogni centroide
      (cons (norm-r (car observations) cs most-positive-fixnum NIL)
              (partition-n (cdr observations) cs))))

(defun norm-r (v cs d-old result)
"Parametro observations, lista di vettori (ovvero liste).
 Parametro c, centroide.
 Calcola la distanza tra ogni vettore di observations ed il centroide c,
 e ritorna una lista di tris (Distanza Centroide Vettore)."
  ;; 
  (if (null cs) result
      ;; 
      (let ((d-new (norm (vsub v (car cs)))))
           (if (< d-new d-old) (norm-r v (cdr cs)
                                         d-new
                                         (cons (car cs)
                                               (cons v NIL)))
               (norm-r v (cdr cs) d-old result)))))

(defun partition-r (observations cs)
"Parametro observations, lista di coppie (Centroide Vettore).
 Parametro cs, lista di centroidi.
 Ritorna la lista di liste di vettori raggruppati per centroide."
  ;; Caso base: non ci sono centroidi da computare
  (if (null cs) NIL
      ;; Calcola la lista di vettori per il primo centroide (rimuovendo gli
      ;; eventuali duplicati) e ricorsivamente per ogni centroide
      (cons (remove-duplicates (partition-a observations (car cs)))
              (partition-r observations (cdr cs)))))

(defun partition-a (observations c)
"Parametro observations, lista di coppie (Centroide Vettore).
 Parametro c, centroide.
 Ritorna la lista di vettori appartenenti alle coppie corrispondenti."
  ;; Caso base: non ci sono coppie da computare
  (if (null observations) NIL
      ;; Estrai il primo vettore avente come centroide corrispondente c
      ;; e ricorsivamente dal cdro di observations (cdr ...)
      (append (cdr (assoc c observations :test #'equal))
              (partition-a (cdr observations) c))))

(defun re-centroids (clusters)
"Parametro clusters, lista di liste di vettori (ovvero liste).
 Ricalcola il centroide di ogni gruppo."
  (mapcar #'centroid clusters))
