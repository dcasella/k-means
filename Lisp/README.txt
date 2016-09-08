Libreria Common Lisp "k-means"



Funzione initialize
Utilizzando il metodo di Forgy per calcolare i centroidi iniziali, c'è la possibilità di ottenere clusters relativamente eterogenei(*), questo è dovuto alla randomizzazione dell'algoritmo che non permette di ottenere risultati costanti.

Funzione partition
L'algoritmo di partenza (e di consegna, nel caso di Lisp) che calcola una lista di lunghezza observations*k, la ordina, e ricorre sulla lista stessa più e più volte per rimuovere man mano gli elementi superflui (o duplicati), risulta comunque più efficiente di un algoritmo che itera notevolmente meno volte e che non manipola una lista di dimensioni tali (observations*k), al contrario della parte Prolog che con l'implementazione di questo algoritmo ha visto un incremento del 1000% nelle prestazioni (testato con 500 punti e 100 k).

--

* http://elearning.unimib.it/mod/forum/discuss.php?d=25261
