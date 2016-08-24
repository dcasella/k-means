# KM-201609
Linguaggi di Programmazione  
AA 2015-2016  
Progetto settembre 2016  
__k-medie__  
Marco Antoniotti e Gabriella Pasi  
DISCo  

## 1 Introduzione
Uno degli algoritmi principali (e più semplici) utilizzati nell'_analisi statistica dei dati_ è noto come l’algoritmo di _clustering_ non _supervisionato_ (“unsupervised”) delle __k-medie__.  
L’obiettivo di un algoritmo di clustering è, dato un insieme di _n oggetti_ (o _osservazioni_), partizionarli in k sottoinsiemi (o categorie non predefinite) che raggruppino oggetti che condividono delle proprietà.  
Ad esempio un algoritmo di clustering applicato a delle immagini telerilevate potrebbe partizionare le immagini sulla base della tipologia di scena rappresentata, quale centri abitati, boschi, superfici acquee, ecc. In particolare, l’algoritmo di clustering delle __k-medie__ è di partizionare n _osservazioni_ in k _clusters_ (gruppi), dove ogni osservazione appartiene al gruppo in cui cade la _media_ più “vicina”. La “media” (detta _centroide_) serve come “prototipo” del gruppo. Il centroide che rappresenta una categoria viene in questo caso calcolato come la media degli oggetti del gruppo e ne costituisce il prototipo.  
In generale il problema è NP-hard, ma la variante “euristica” di Lloyd dell’algoritmo __k-medie__ è una soluzione abbastanza buona ed efficace. Una limitazione dell’algoritmo __k-medie__ è che il parametro k deve
essere specificato dall’utente in anticipo.  
Il vostro compito è di construire una libreria `Common Lisp` ed una libreria `Prolog` che implementino l’algoritmo __k-medie__ di Lloyd.  
Per una descrizione dell’algoritmo delle __k-medie__ potete guardare G. James, D. Witten, T. Hastie, R. Tibshirani, _An Introduction to Statistical Learning_, Springer, 2015, o al più avanzato T. Hastie, R. Tibshirani, J. Friedman, _The Elements of Statistical Learning, Data Mining, Inference, and Prediction_,
Springer, 2009, oppure anche le descrizioni di Wikipedia (Inglese).  
L’Algoritmo 1 rappresenta (in pseudo codice) i passi principali dell’algoritmo __k-medie__.  
___
__Algoritmo 1 k-medie__ di Lloyd: pseudo codice.  
```js
KM(n observations, k) → k clusters
1: cs ← Initialize(k)
   Crea k centroidi iniziali, ad esempio usando il metodo di Forgy che sceglie casualmente k delle osservazioni iniziali.
2: clusters ← {}
3: clusters0 ← Partition(observations, cs)
   Raggruppa le “observations” attorno ai k centroidi in “cs”.
4: if clusters = clusters0 then
5:     return clusters
6: else
7:     clusters ← clusters0
8:     cs ← RecomputeCentroids(clusters)
       Ricalcola il “centroide” di ogni gruppo.
9:     goto 3
10: end if
```
___

## 2 Requisiti Progetto
Innanzitutto il progetto è realizzabile in modo completamente funzionale (o logico). Non potete usare operazioni di assegnamento (__set__, __setq__, __setf__) o asserzioni sulla base dati `Prolog`, se non per casi assolutamente necessari e dopo aver ricevuto esplicito permesso.  

### Osservazioni
Le “osservazioni” sono, nel caso più semplice, dei vettori numerici. Sempre per semplicità in `Common Lisp` ed in `Prolog` dovrete rappresentare le “osservazioni” con delle _liste_.  
Dato che il cuore dell’algoritmo __k-medie__ è costituito da un’operazione di calcolo di una distanza (Euclidea) tra vettori, dovrete implementare una serie di operazioni vettoriali.  
* somma, sottrazione, prodotto scalare
* prodotto interno (euclideo)
* norma

### Esempi Common Lisp
Creiamo un vettore v3
```lisp
CL prompt> (defparameter v3 (list 1 2 3))
V3

CL prompt> v3
(1 2 3)
```
Ora calcoliamo la sua norma, ovvero...
```lisp
CL prompt> (srqt (innerprod V3 V3))
3.7416575 ; Il risultato può variare.
```
dove `innerprod` è il prodotto interno. Naturalmente possiamo anche avere
```lisp
CL prompt> (norm V3)
3.7416575
```
Somme, etc...
```lisp
CL prompt> (vsum V3 (list 10 0 42))
(11 2 45)
```
Le funzioni map, mapc, mapcar, mapcan, reduce etc., sono più che utili in questo frangente.

### Esempi Prolog
Ricordiamoci un vettore...
```prolog
?- new_vector(v3, [1, 2, 3]).
true

?- vector(v3, V).
V = [1, 2, 3]
```
Ora calcoliamo la sua norma...
```prolog
?- vector(v3, V), innerprod(V, V, IP), N is sqrt(IP).
V = [1, 2, 3]
N = 3.7416575

?- vector(v3, V), norm(V, N).
V = [1, 2, 3]
N = 3.7416575

?- vector(v3, V), vsum(V, [10, 0, 42], S).
V = [1, 2, 3]
S = [11, 2, 45]
```
