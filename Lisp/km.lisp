;;;; -*- Mode: Lisp -*-

;;;; km.lisp --
;;;; Casella Davide 793631
;;;; Nicolini Fabio 794467



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

(defun centroid (observations)
 "Parametro observations, lista di vettori (ovvero liste).
 Calcola il centroide dell'insieme di osservazioni observations."
  ;; Dividi ogni coordinata del vettore generato da reduce per il numero
  ;; di osservazioni
  (mapcar #'(lambda (coord) (/ coord (length observations)))
          ;; Somma (facendo uso di vsum) le osservazioni
          (reduce #'vsum observations)))

(defun innerprod (vector1 vector2)
 "Parametro vector1, vettore (lista di coordinate).
 Parametro vector2, vettore (lista di coordinate).
 Calcola il prodotto interno di due vettori."
  (reduce #'+ (mapcar #'* vector1 vector2)))

(defun norm (vector)
 "Parametro vector, vettore (lista di coordinate).
 Calcola la norma euclidea di un vettore."
  (sqrt (innerprod vector vector)))

(defun initialize (observations k)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro k, numero di clusters da generare.
 Crea k centroidi iniziali usando il metodo di Forgy.
 Metodo di Forgy: sceglie casualmente k delle osservazioni iniziali."
  ;; Caso base: la lista risultante è composta da k vettori
  (cond ((= k 0) NIL)
        ;; rand = Vettore estratto da observations dato un indice casuale
        (T (let ((rand (nth (random (length observations)) observations)))
                ;; Rimuovi il vettore selezionato da observations
                ;; per non incorrerci nuovamente nelle ricorsioni future
                (cons rand (initialize (remove rand observations) (- k 1)))))))

(defun re-centroids (clusters)
 "Parametro clusters, lista di liste di vettori (ovvero liste).
 Ricalcola il centroide di ogni gruppo."
  (mapcar #'centroid clusters))

(defun norm-r (v cs old-distance result)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro c, centroide.
 Ritorna la coppia (Centroide Vettore) la cui distanza tra vettore e centroide
 è la minore calcolata."
  ;; Caso base: non ci sono centroidi da computare, restituisci result
  (cond ((null cs) result)
        ;; Calcola la distanza tra il centroide attuale e il vettore
        (T (let* ((c (car cs)) (new-distance (norm (vsub v c))))
                 ;; Controlla se è minore dell'ultima distanza calcolata
                 (cond ((< new-distance old-distance)
                        ;; Aggiorna result con la coppia (Centroide Vettore)
                        ;; e continua con la ricorsione
                        (norm-r v (cdr cs) new-distance (list c v)))
                       ;; Mantieni result e continua con la ricorsione
                       (T (norm-r v (cdr cs) old-distance result)))))))

(defun partition-n (observations cs)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro cs, lista di centroidi.
 Ritorna la lista di liste di coppie (Centroide Vettore)."
  ;; Caso base: non ci sono vettori da computare
  (cond ((null observations) NIL)
        ;; Calcola la coppia (Centroide Vettore) per il primo vettore
        ;; e ricorsivamente per il resto di observations
        (T (cons (norm-r (car observations) cs most-positive-fixnum NIL)
                 (partition-n (cdr observations) cs)))))

(defun partition-a (observations c)
 "Parametro observations, lista di coppie (Centroide Vettore).
 Parametro c, centroide.
 Ritorna la lista di vettori appartenenti al centroide c."
  ;; Caso base: non ci sono coppie da computare
  (cond ((null observations) NIL)
        ;; Estrai il primo vettore avente come centroide corrispondente c
        ;; e ricorsivamente dal resto di observations
        (T (append (cdr (assoc c observations :test #'equal))
                   (partition-a (cdr observations) c)))))

(defun partition-r (observations cs)
 "Parametro observations, lista di coppie (Centroide Vettore).
 Parametro cs, lista di centroidi.
 Ritorna la lista di liste di vettori raggruppati per centroide."
  ;; Caso base: non ci sono centroidi da computare
  (cond ((null cs) NIL)
        ;; Calcola la lista di vettori per il primo centroide (rimuovendo gli
        ;; eventuali duplicati) e ricorsivamente per ogni centroide
        (T (cons (remove-duplicates (partition-a observations (car cs)))
                 (partition-r observations (cdr cs))))))

(defun list< (a b)
 "Parametro a, coppia (Centroide Vettore).
 Parametro b, coppia (Centroide Vettore).
 Ordina in base al Centroide le due coppie (ovvero liste)"
  ;; Caso base: se a è NIL e b non lo è, ritorna True
  (cond ((and (null a) (not (null b))) T)
        ;; Caso base: se b è NIL, ritorna NIL
        ((null b) NIL)
        ;; Se le prime coordinate di a e b sono uguali, ordina ricorsivamente
        ;; secondo il resto delle coordinate
        ((= (caar a) (caar b)) (list< (cdr a) (cdr b)))
        ;; Ordina a < b quando le prime coordinate a loro volta sono una
        ;; minore dell'altra
        (T (< (caar a) (caar b)))))

(defun partition (observations cs)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro cs, lista di centroidi.
 Raggruppa le observations attorno ai k centroidi in cs."
  ;; Calcola la lista di liste di coppie (Centroide Vettore)
  ;; Ordina il risultato di partition-n secondo il centroide
  ;; Raggruppa le coppie (Centroide Vettore) in liste di vettori (clusters)
  (partition-r (sort (partition-n observations cs) #'list<) cs))

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
       (cond ((equal clusters new-clusters) clusters)
             ;; Computa ricorsivamente i clusters con nuovi centroidi
             (T (km-r observations
                      new-clusters
                      (re-centroids new-clusters))))))

(defun km (observations k)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro k, numero di clusters da generare.
 Ritorna k clusters dell'insieme di osservazioni observations."
  ;; Controlla se il numero di osservazioni è minore di k
  (cond ((< (length observations) k) (error "Can't compute clusters."))
        ;; Controlla se il numero di osservazioni è uguale a k
        ((= (length observations) k) observations)
        ;; Controlla se non è possibile computare observations
        ((null observations) NIL)
        ;; Prosegui con l'algoritmo
        (T (km-r observations NIL (initialize observations k)))))

;;;; end of file -- km.lisp --
