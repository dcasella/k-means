Libreria Common Lisp "k-means"

Casella Davide 793631
Nicolini Fabio 794467



Funzione initialize
Utilizzando il metodo di Forgy per calcolare i centroidi iniziali, c'è la possibilità di ottenere clusters relativamente eterogenei(*), questo è dovuto alla randomizzazione dell'algoritmo che non permette di ottenere risultati costanti.

Funzione partition
L'algoritmo calcola per ogni Vettore la distanza con ogni Centroide, assegnando l'indice del Cluster alla posizione del Vettore, ovvero andando a formare una Clusters-Map. Nel testing di questo algoritmo ci siamo imbattuti nel problema dei Clusters vuoti(**): abbiamo gestito quest'ultimo risceivendo il codice di alcuni casi base per ovviare agli errori a run-time, restituendo così liste vuote - NIL - per ogni Cluster vuoto.
La decisione di implementare la Clusters-Map è derivata dal fatto che usare la ricorsione per computare ogni elemento (ovvero una lista di liste) di una lista risulta notevolmente più lento con una quantità non trascurabile di elementi di input.

Funzione map-cluster
(map-cluster <clusters-map> <observations> <cluster-index> <index>)
Dato un indice di Cluster (cluster-index, o cl nel codice), la funzione ricorre sulla lista clusters-map: per ogni elemento, controlla il rispettivo valore (indice di Cluster) e, se uguale all'indice cercato (cluster-index) aggiunge il vettore (usando la funzione nth su observations) alla lista risultante.
Il parametro index è necessario poichè l'indice corrente dell'elemento viene perso durante le ricorsioni.

Funzione map-clusters
(map-clusters <clusters-map> <observations> <cluster-index> <k>)
Genera K Clusters, eseguendo la funzione map-cluster per ogni indice di Cluster (cluster-index, o cl nel codice) fino ad un massimo di K, ovvero il numero di Clusters.

Avendo optato per l'utilizzo di una Clusters-Map, sono state apportate le dovute modifiche alle seguenti funzioni:
re-centroids, all'interno della funzione mapcar è necessario l'uso di map-clusters;
km, il risultato viene restituito dopo l'uso di map-clusters.

--

* Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25261 e http://elearning.unimib.it/mod/forum/discuss.php?d=25489
** Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25420
