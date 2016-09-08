Libreria Common Lisp "k-means"

Casella Davide 793631
Nicolini Fabio 794467


Funzione initialize
Utilizzando il metodo di Forgy per calcolare i centroidi iniziali, c'è la possibilità di ottenere clusters relativamente eterogenei(*), questo è dovuto alla randomizzazione dell'algoritmo che non permette di ottenere risultati costanti.

Funzione partition
L'algoritmo calcola per ogni Vettore la distanza con ogni Centroide, e procede con l'esecuzione con le coppie (Centroide Vettore) di distanza minore tra di loro. La lista di coppie viene ordinata per centroide così da rendere più piacevole il raggruppamento. Dalla lista risultante vengono estratti i Vettori appartenenti ad ogni gruppo (cluster per cluster).

--

* Riferimento a http://elearning.unimib.it/mod/forum/discuss.php?d=25261
