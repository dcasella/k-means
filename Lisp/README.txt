Libreria Common Lisp "k-means"

Vettore:
(coordinata+)
Esempio vettore 3D:
(4.2 42 -0.42)

Osservazioni:
(vettore+)
Esempio:
CL-USER 1 > (defparameter observations '((3.0 7.0) (0.5 1.0) (0.8 0.5) (1.0 8.0) (0.9 1.2) (6.0 4.0) (7.0 5.5) (4.0 9.0) (9.0 4.0)))
OBSERVATIONS

Funzioni principali:
(km <observations> <k>)
<observations> è una lista di vettori.
<k> è il numero di clusters da generare.
La funzione km implementa l'algoritmo k-medie di Lloyd, che calcola <k> clusters di vettori raggruppati dalle osservazioni <observations>.
Chiama km-r (con argomenti <clusters> NIL e <cs> risultato della funzione initialize) dopo aver stabilito che il numero di osservazioni sia maggiore o uguale a <k>.
Esempio:
CL-USER 2 > (km observations 3)
(((3.0 7.0) (4.0 9.0) (1.0 8.0)) ((7.0 5.5) (6.0 4.0) (9.0 4.0)) ((0.5 1.0) (0.9 1.2) (0.8 0.5)))

(centroid <observations>)
<observations> è una lista di vettori.
La funzione centroid ritorna il centroide dell’insieme di osservazioni <observations>.
Esempio:
CL-USER 3 > (centroid '((3.0 7.0) (4.0 9.0) (1.0 8.0)))
(2.6666668 8.0)

(vsum <vector1> <vector2>)
<vector1> è un vettore.
<vector2> è un vettore.
La funzione vsum calcola la somma vettoriale di due vettori.
Esempio:
CL-USER 4 > (vsum '(4 2) '(-46 40))
(-42 42)

(vsub <vector1> <vector2>)
<vector1> è un vettore.
<vector2> è un vettore.
La funzione vsub calcola la differenza vettoriale di due vettori.
Esempio:
CL-USER 5 > (vsub '(4 2) '(46 -40))
(-42 42)

(innerprod <vector1> <vector2>)
<vector1> è un vettore.
<vector2> è un vettore.
La funzione innerprod calcola il prodotto interno di due vettori.
Esempio:
CL-USER 6 > (innerprod '(4 2) '(8 5))
42

(norm <vector>)
<vector> è un vettore.
La funzione norm calcola la norma euclidea di un vettore.
Esempio:
CL-USER 7 > (norm '(4.2 42))
42.209477

Funzioni ausiliarie:
(initialize <observations> <k>)
<observations> è una lista di vettori.
<k> è il numero di clusters da generare.
La funzione initialize crea <k> centroidi iniziali usando il metodo di Forgy.
Il metodo di Forgy consiste nello scegliere casualmente <k> delle osservazioni <observations> iniziali.
Esempio:
CL-USER 8 > (initialize observations 3)
((1.0 8.0) (9.0 4.0) (0.8 0.5))

(km-r <observations> <clusters> <cs>)
<observations> è una lista di vettori.
<clusters> è la lista di gruppi di vettori della ricorsione precedente (o NIL).
<cs> è la lista di centroidi.
La funzione km-r calcola la lista di gruppi di vettori chiamando partition.
Se <new-clusters> (calcolati da partition) risulta essere uguale a <clusters>, la ricorsione termina e <clusters> (o <new-clusters>) è il risultato.
Chiama ricorsivamente km-r con argomenti <observations>, <new-clusters> e il risultato della funzione re-centroids.
Esempio:
CL-USER 9 > (km-r observations NIL '((1.0 8.0) (9.0 4.0) (0.8 0.5)))
(((7.0 5.5) (6.0 4.0) (9.0 4.0)) ((0.5 1.0) (0.9 1.2) (0.8 0.5)) ((3.0 7.0) (4.0 9.0) (1.0 8.0)))

(partition <observations> <cs>)
<observations> è una lista di vettori.
<cs> è la lista di centroidi.
La funzione partition raggruppa le osservazioni <observations> attorno ai k centroidi <cs>.
Calcola la lista di liste di tris (Distanza Centroide Vettore) con la funzione partition-n.
Ordina il risultato di partition-n secondo la Distanza con la funzione sort.
Rimuove i vettori (#'third) duplicati a partire dal fondo con la funzione remove-duplicates.
Rimuove la Distanza dai tris (diventando così coppie) con la funzione remove-first.
Raggruppa le coppie (Centroide Vettore) in liste di vettori (clusters) con la funzione partition-r.
Esempio:
CL-USER 9 > (partition observations '((1.0 8.0) (9.0 4.0) (0.8 0.5)))
(((1.0 8.0) (3.0 7.0) (4.0 9.0)) ((9.0 4.0) (7.0 5.5) (6.0 4.0)) ((0.8 0.5) (0.5 1.0) (0.9 1.2)))

(partition-n <observations> <cs>)
<observations> è una lista di vettori.
<cs> è la lista di centroidi.
La funzione partition-n calcola la lista di liste di tris (Distanza Centroide Vettore).
Calcola la lista di tris per il primo centroide con la funzione norm-r e ricorsivamente per ogni centroide.
Esempio:
CL-USER 10 > (partition-n observations '((1.0 8.0) (9.0 4.0) (0.8 0.5)))
((2.236068 (1.0 8.0) (3.0 7.0)) (7.017834 (1.0 8.0) (0.5 1.0)) (7.5026665 (1.0 8.0) (0.8 0.5)) (0.0 (1.0 8.0) (1.0 8.0)) (6.8007355 (1.0 8.0) (0.9 1.2)) (6.4031243 (1.0 8.0) (6.0 4.0)) (6.5 (1.0 8.0) (7.0 5.5)) (3.1622777 (1.0 8.0) (4.0 9.0)) (8.944272 (1.0 8.0) (9.0 4.0)) (6.708204 (9.0 4.0) (3.0 7.0)) (9.013878 (9.0 4.0) (0.5 1.0)) (8.915716 (9.0 4.0) (0.8 0.5)) (8.944272 (9.0 4.0) (1.0 8.0)) (8.570298 (9.0 4.0) (0.9 1.2)) (3.0 (9.0 4.0) (6.0 4.0)) (2.5 (9.0 4.0) (7.0 5.5)) (7.071068 (9.0 4.0) (4.0 9.0)) (0.0 (9.0 4.0) (9.0 4.0)) (6.8622155 (0.8 0.5) (3.0 7.0)) (0.5830952 (0.8 0.5) (0.5 1.0)) (0.0 (0.8 0.5) (0.8 0.5)) (7.5026665 (0.8 0.5) (1.0 8.0)) (0.7071068 (0.8 0.5) (0.9 1.2)) (6.268173 (0.8 0.5) (6.0 4.0)) (7.964923 (0.8 0.5) (7.0 5.5)) (9.0824 (0.8 0.5) (4.0 9.0)) (8.915716 (0.8 0.5) (9.0 4.0)))

(norm-r <observations> <c>)
<observations> è una lista di vettori.
<c> è il centroide.
La funzione norm-r calcola la distanza tra ogni vettore di <observations> ed il centroide <c>, e ritorna una lista di tris (Distanza Centroide Vettore).
Esempio:
CL-USER 11 > (norm-r observations '(1.0 8.0))
((2.236068 (1.0 8.0) (3.0 7.0)) (7.017834 (1.0 8.0) (0.5 1.0)) (7.5026665 (1.0 8.0) (0.8 0.5)) (0.0 (1.0 8.0) (1.0 8.0)) (6.8007355 (1.0 8.0) (0.9 1.2)) (6.4031243 (1.0 8.0) (6.0 4.0)) (6.5 (1.0 8.0) (7.0 5.5)) (3.1622777 (1.0 8.0) (4.0 9.0)) (8.944272 (1.0 8.0) (9.0 4.0)))

(remove-first <observations>)
<observations> è una lista di tris (Distanza Centroide Vettore).
La funzione remove-first rimuove il primo elemento di ogni sotto-lista di <observations>.
Estrae il resto della prima sotto-lista appartenente a <observations> e ricorsivamente da ogni sotto-lista.
Esempio:
CL-USER 12 > (remove-first (remove-duplicates (sort (partition-n observations '((1.0 8.0) (9.0 4.0) (0.8 0.5))) #'< :key #'car) :key #'third :from-end T))
(((1.0 8.0) (1.0 8.0)) ((9.0 4.0) (9.0 4.0)) ((0.8 0.5) (0.8 0.5)) ((0.8 0.5) (0.5 1.0)) ((0.8 0.5) (0.9 1.2)) ((1.0 8.0) (3.0 7.0)) ((9.0 4.0) (7.0 5.5)) ((9.0 4.0) (6.0 4.0)) ((1.0 8.0) (4.0 9.0)))

(partition-r <observations> <cs>)
<observations> è una lista di coppie (Centroide Vettore).
<cs> è la lista di centroidi.
La funzione partition-n calcola la lista di liste di vettori raggruppati per centroide (clusters).
Calcola la lista di vettori per il primo centroide con la funzione partition-a (rimuovendo gli eventuali duplicati con la funzione remove-duplicates) e ricorsivamente per ogni centroide.
Esempio:
CL-USER 13 > (partition-r (remove-first (remove-duplicates (sort (partition-n observations '((1.0 8.0) (9.0 4.0) (0.8 0.5))) #'< :key #'car) :key #'third :from-end T)) '((1.0 8.0) (9.0 4.0) (0.8 0.5)))
(((1.0 8.0) (3.0 7.0) (4.0 9.0)) ((9.0 4.0) (7.0 5.5) (6.0 4.0)) ((0.8 0.5) (0.5 1.0) (0.9 1.2)))

(partition-a <observations> <c>)
<observations> è una lista di coppie (Centroide Vettore).
<c> è il centroide.
La funzione partition-a calcola la lista di vettori appartenenti alle coppie corrispondenti.
Estrae il primo vettore avente come centroide corrispondente <c> con la funzione assoc e ricorsivamente dal resto di <observations>.
Esempio:
CL-USER 14 > (partition-a (remove-first (remove-duplicates (sort (partition-n observations '((1.0 8.0) (9.0 4.0) (0.8 0.5))) #'< :key #'car) :key #'third :from-end T)) '(1.0 8.0))
((1.0 8.0) (3.0 7.0) (3.0 7.0) (3.0 7.0) (3.0 7.0) (3.0 7.0) (4.0 9.0) (4.0 9.0) (4.0 9.0))

(re-centroids <clusters>)
<clusters> è una lista di liste di vettori.
La funzione re-centroids calcola il centroide di ogni lista di vettori (cluster).
Esempio:
CL-USER 15 > (re-centroids (partition observations '((1.0 8.0) (9.0 4.0) (0.8 0.5))))
((2.6666668 8.0) (7.3333335 4.5) (0.7333333 0.90000004))
