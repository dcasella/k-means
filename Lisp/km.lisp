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
	(cond ((null observations) NIL)
	      (T (mapcar #'(lambda (coord) (/ coord (length observations)))
									 ;; Somma (facendo uso di vsum) le osservazioni
									 (reduce #'vsum observations)))))

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

(defun map-cluster (clusters observations cl index)
 "Parametro clusters, lista di indici.
 Parametro observations, lista di vettori (ovvero liste).
 Parametro cl, indice del cluster.
 Parametro index, indice di (car clusters) rispetto a clusters.
 Ritorna la lista di vettori rappresentante il cluster di indice cl."
	;; Caso base: non ci sono indici da computare
	(cond ((null clusters) NIL)
				;; Controlla se cl è uguale all'indice dell'elemento nella Clusters-Map
				((not (= cl (car clusters)))
				 ;; Continua la ricorsione
				 (map-cluster (cdr clusters) observations cl (+ index 1)))
				;; Ricava il vettore di indice index (con indice del cluster uguale a cl)
				;; da observations e ricorsivamente per il resto di clusters
				(T (cons (nth index observations)
								 (map-cluster (cdr clusters) observations cl (+ index 1))))))

(defun map-clusters (clusters observations cl k)
 "Parametro clusters, lista di indici.
 Parametro observations, lista di vettori (ovvero liste).
 Parametro cl, indice di (car clusters) rispetto a clusters.
 Parametro k, numero di clusters da generare.
 Ritorna la lista di clusters partendo da una lista di indici di clusters."
  ;; Caso base: l'indice di cluster è uguale a k (ricorsione terminata)
	(cond ((= cl k) NIL)
				;; Ricava il cluster (di indice cl) di vettori e ricorsivamente per il
				;; resto dei clusters (di indici maggiori di cl)
				(T (cons (map-cluster clusters observations cl 0)
								 (map-clusters clusters observations (+ cl 1) k)))))

(defun re-centroids (clusters observations k)
 "Parametro clusters, lista di liste di vettori (ovvero liste).
 Parametro observations, lista di vettori (ovvero liste).
 Parametro cl, indice del cluster.
 Parametro k, numero di clusters da generare.
 Ricalcola il centroide di ogni cluster."
	(mapcar #'centroid (map-clusters clusters observations 0 k)))

(defun pick-centroid (v cs old-distance result)
 "Parametro v, vettore.
 Parametro cs, lista di centroidi.
 Parametro old-distance, ultima distanza calcolata nella ricorsione.
 Parametro result, centroide, possibile risultato della funzione.
 Ritorna il centroide la cui distanza tra v e centroide è la minore calcolata."
	;; Caso base: non ci sono centroidi da computare, restituisci result
	(cond ((null cs) result)
	      ((null (car cs)) (pick-centroid v (cdr cs) old-distance result))
				;; Calcola la distanza tra il centroide attuale e il vettore
				(T (let* ((c (car cs)) (new-distance (norm (vsub v c))))
								 ;; Controlla se è minore dell'ultima distanza calcolata
								 (cond ((< new-distance old-distance)
												;; Aggiorna result con il centroide
												;; e continua con la ricorsione
												(pick-centroid v (cdr cs) new-distance c))
											 ;; Mantieni result e continua con la ricorsione
											 (T (pick-centroid v (cdr cs) old-distance result)))))))

(defun partition (observations cs)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro cs, lista di centroidi.
 Ritorna la Clusters-Map, ovvero una lista di indici di clusters,
 dati cs come centroidi."
	;; Caso base: non ci sono vettori da computare
	(cond ((null observations) NIL)
				;; Calcola il centroide migliore per il primo vettore,
				;; ricava l'indice del centroide nella lista cs,
				;; e ricorsivamente per il resto di observations
				(T (cons (position (pick-centroid (car observations)
																					cs
																					most-positive-fixnum
																					cs) cs)
								 (partition (cdr observations) cs)))))

(defun lloyd-km (observations clusters cs k)
 "Parametro observations, lista di vettori (ovvero liste).
 Parametro clusters, lista di gruppi di vettori calcolati nella ricorsione
 precedente (NIL durante la prima chiamata).
 Parametro cs, lista di centroidi.
 Parametro k, numero di clusters da generare.
 Ritorna la Clusters-Map, ovvero una lista di indici di clusters."
	;; new-clusters = Lista di gruppi di vettori ottenuta raggruppando
	;; le observations attorno ai centroidi in cs
	(let ((new-clusters (partition observations cs)))
			 ;; Caso base: i clusters calcolati nella ricorsione attuale sono
			 ;; uguali a quelli calcolati nella ricorsione precedente
			 (cond ((equal clusters new-clusters) clusters)
						 ;; Computa ricorsivamente i clusters con nuovi centroidi
						 (T (lloyd-km observations
													new-clusters
													(re-centroids new-clusters observations k)
													k)))))

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
				(T (map-clusters (lloyd-km observations
																	 NIL
																	 (initialize observations k)
																	 k)
												 observations 0 k))))

;;;; end of file -- km.lisp --
