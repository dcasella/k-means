Libreria Prolog "k-means"

Casella Davide 793631
Nicolini Fabio 794467



Predicato initialize
Utilizzando il metodo di Forgy per calcolare i centroidi iniziali, c'è la possibilità di ottenere clusters relativamente eterogenei(*), questo è dovuto alla randomizzazione dell'algoritmo che non permette di ottenere risultati costanti.

Predicato partition
L'algoritmo calcola per ogni Vettore la distanza con ogni Centroide, assegnando l'indice del Cluster alla posizione del Vettore, ovvero andando a formare una Clusters-Map. Nel testing di questo algoritmo ci siamo imbattuti nel problema dei Clusters vuoti(**): abbiamo gestito quest'ultimo risceivendo il codice di alcuni casi base per ovviare agli errori a run-time, restituendo così liste vuote - [] - per ogni Cluster vuoto.

--

* Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25261 e http://elearning.unimib.it/mod/forum/discuss.php?d=25489
** Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25420
