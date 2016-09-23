Libreria Prolog "k-means"

Casella Davide 793631
Nicolini Fabio 794467



Predicato initialize
Utilizzando il metodo di Forgy per calcolare i centroidi iniziali, c'è la possibilità di ottenere clusters relativamente eterogenei(*), questo è dovuto alla randomizzazione dell'algoritmo che non permette di ottenere risultati costanti.

Predicato partition
L'algoritmo calcola per ogni Vettore la distanza con ogni Centroide, assegnando l'indice del Cluster alla posizione del Vettore, ovvero andando a formare una Clusters-Map. Nel testing di questo algoritmo ci siamo imbattuti nel problema dei Clusters vuoti(**): abbiamo gestito quest'ultimo riscrivendo il codice di alcuni casi base per ovviare agli errori a run-time, restituendo così liste vuote - [] - per ogni Cluster vuoto.
La decisione di implementare la Clusters-Map è derivata dal fatto che usare la ricorsione per computare ogni elemento (ovvero una lista di liste) di una lista risulta notevolmente più lento con una quantità non trascurabile di elementi di input.

Predicato map_cluster
map_cluster(+ClustersMap, +Observations, +CL, +Index, -Result)
Dato un indice di Cluster (ClusterIndex, o CL nel codice), la funzione ricorre sulla lista ClustersMap: per ogni elemento, controlla il rispettivo valore (indice di Cluster) e, se uguale all'indice cercato (ClusterIndex) aggiunge il vettore (usando la funzione nth0 su Observations) alla lista risultante.
Il parametro Index è necessario poichè l'indice corrente dell'elemento viene perso durante le ricorsioni.

Predicato map_clusters
map_cluster(+ClustersMap, +Observations, +CL, +K, -Result)
Genera K Clusters, eseguendo la funzione map_cluster per ogni indice di Cluster (ClusterIndex, o CL nel codice) fino ad un massimo di K, ovvero il numero di Clusters.

Avendo optato per l'utilizzo di una Clusters-Map, sono state apportate le dovute modifiche ai seguenti predicati:
re_centroids, prima dell'utilizzo del predicato maplist, è necessario l'uso di map_clusters;
km, il risultato viene restituito dopo l'uso di map_clusters.

Altri predicati di supporto sono i seguenti:
vsum_list, traducibile con una reduce in Lisp;
identity, necessario per la computazione di vsum_list;
vdiv, divide per L ogni elemento della lista di input;
vprod, moltiplica due Vettori tra di loro;
vector, controlla che la lista di input sia un vettore valido.

--

* Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25261 e http://elearning.unimib.it/mod/forum/discuss.php?d=25489
** Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25420
