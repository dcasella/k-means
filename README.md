# K-Means

Uno degli algoritmi principali (e più semplici) utilizzati nell'_analisi statistica dei dati_ è noto come l’algoritmo di _clustering_ non _supervisionato_ (“unsupervised”) delle __k-medie__.  
L’obiettivo di un algoritmo di clustering è, dato un insieme di _n oggetti_ (o _osservazioni_), partizionarli in _k_ sottoinsiemi (o categorie non predefinite) che raggruppino oggetti che condividono delle proprietà.  
Ad esempio un algoritmo di clustering applicato a delle immagini telerilevate potrebbe partizionare le immagini sulla base della tipologia di scena rappresentata, quale centri abitati, boschi, superfici acquee, ecc. In particolare, l’algoritmo di clustering delle __k-medie__ è di partizionare n _osservazioni_ in _k clusters_ (gruppi), dove ogni osservazione appartiene al gruppo in cui cade la _media_ più “vicina”. La “media” (detta _centroide_) serve come “prototipo” del gruppo. Il centroide che rappresenta una categoria viene in questo caso calcolato come la media degli oggetti del gruppo e ne costituisce il prototipo.  
In generale il problema è NP-hard, ma la variante “euristica” di Lloyd dell’algoritmo __k-medie__ è una soluzione abbastanza buona ed efficace. Una limitazione dell’algoritmo __k-medie__ è che il parametro _k_ deve
essere specificato dall’utente in anticipo.


___

__Algoritmo 1 k-medie__ di Lloyd: pseudo codice.  
```js
KM(n observations, k) → k clusters
1: cs ← Initialize(k)
2: clusters ← {}
3: clusters0 ← Partition(observations, cs)
4: if clusters = clusters0 then
5:     return clusters
6: else
7:     clusters ← clusters0
8:     cs ← RecomputeCentroids(clusters)
9:     goto 3
10: end if
```
___


## List of contents

- [Interface](#interface)
- [Examples](#examples)


## Interface

### Common Lisp

__km__ _observations k_ → _clusters_  

Il parametro _observations_ è una lista di vettori (ovvero liste), il parametro _k_ è il numero di clusters da generare. Il risultato _clusters_ è una lista di gruppi, ovvero di liste di vettori (che, ripetiamo, sono liste).
La funzione deve fallire se il numero di osservazioni è minore di _k_.
___

__centroid__ _observations_ → _centroid_  

La funzione __centroid__ ritorna il centroide (i.e., la “media”) dell’insieme di osservazioni _observations_ (una lista di vettori, ovvero di altre liste).  
Nota bene. Il centroide di un insieme di vettori non è necessariamente un elemento dell’insieme dato.
___

__vsum__ _vector1 vector2_ → _v_  

La funzione __vsum__ calcola la somma (vettoriale) di due vettori.
___

__vsub__ _vector1 vector2_ → _v_  

La funzione __vsub__ calcola la differenza (vettoriale) di due vettori.
___

__innerprod__ _vector1 vector2_ → _v_  

La funzione __innerprod__ calcola il prodotto interno (vettoriale) di due vettori. Il valore ritornato v è uno scalare.
___

__norm__ _vector_ → _v_  

La funzione __norm__ calcola la norma euclidea di un vettore. Il valore ritornato _v_ è uno scalare.
___


### Prolog

__km__(_Observations_, _K_, _Clusters_)  

Il parametro _Observations_ è una lista di vettori (ovvero liste), il parametro _K_ è il numero di clusters da generare. Il predicato __km/3__ è vero quando _Clusters_ è una lista di gruppi che corrisponde alla partizione
di _Observations_ in _k_ clusters.  
Il predicato __km/3__ deve fallire se il numero di osservazioni è minore di _K_.
___

__centroid__(_Observations_, _Centroid_)  

Il predicato __centroid/2__ è vero quando _Centroid_ è il centroide (i.e., la “media”) dell’insieme di osservazioni _Observations_ (una lista di vettori, ovvero di altre liste).  
Nota bene. Il centroide di un insieme di vettori non è necessariamente un elemento dell’insieme dato.
___

__vsum__(_Vector1_, _Vector2_, _V_)  

Il predicato __vsum/3__ è vero quando _V_ è la somma (vettoriale) di due vettori.
___

__vsub__(_Vector1_, _Vector2_, _V_)  

Il predicato __vsub/3__ è vero quando _V_ è la sottrazione (vettoriale) del vettore _Vector2_ da _Vector1_.
___

__innerprod__(_Vector1_, _Vector2_, _R_)  

Il predicato __innerprod/3__ è vero quando _R_ è il prodotto interno (vettoriale) di due vettori. Il valore _R_ è uno scalare.
___

__norm__(Vector, _N_)  

Il predicato __norm/2__ è vero quando _N_ è la norma euclidea di un vettore. Il valore ritornato _N_ è uno scalare.
___

__new_vector__(_Name_, _Vector_)  

Il predicato __new_vector/2__ è vero quando a _Name_ (un atomo `Prolog`) viene associato un vettore _Vector_.  
In questo caso potete usare __assert__.
___


## Examples

Consider the set of 2D Observations:
```
O = {(3.0, 7.0), (0.5, 1.0), (0.8, 0.5), (1.0, 8.0),
     (0.9, 1.2), (6.0, 4.0), (7.0, 5.5),
     (4.0, 9.0), (9.0, 4.0)}.
```
The three clusters (with k = 3) calculated with the K-Means algorithm are:
```
1. {(1.0, 8.0), (3.0, 7.0), (4.0, 9.0)},
2. {(0.5, 1.0), (0.8, 0.5), (0.9, 1.2)},
3. {(6.0, 4.0), (7.0, 5.5), (9.0, 4.0)}.
```

### Common Lisp

```lisp
CL prompt> (defparameter v3 (list 1 2 3))
V3

CL prompt> v3
(1 2 3)

CL prompt> (srqt (innerprod V3 V3))
3.7416575 ; Il risultato può variare.

CL prompt> (norm V3)
3.7416575

CL prompt> (vsum V3 (list 10 0 42))
(11 2 45)

CL prompt> (defparameter observations
                         ’((3.0 7.0) (0.5 1.0) (0.8 0.5)
                           (1.0 8.0) (0.9 1.2) (6.0 4.0)
                           (7.0 5.5) (4.0 9.0) (9.0 4.0)))

CL prompt> (km observations 3)
(((1.0 8.0) (3.0 7.0) (4.0 9.0))
 ((0.5 1.0) (0.8 0.5) (0.9 1.2))
 ((6.0 4.0) (7.0 5.5) (9.0 4.0)))
```


### Prolog

```prolog
?- new_vector(v3, [1, 2, 3]).
true.

?- vector(v3, V).
V = [1, 2, 3].

?- vector(v3, V), innerprod(V, V, IP), N is sqrt(IP).
V = [1, 2, 3].
N = 3.7416575.

?- vector(v3, V), norm(V, N).
V = [1, 2, 3].
N = 3.7416575.

?- vector(v3, V), vsum(V, [10, 0, 42], S).
V = [1, 2, 3].
S = [11, 2, 45].

?- km([[3.0, 7.0], [0.5, 1.0], [0.8, 0.5],
       [1.0, 8.0], [0.9, 1.2], [6.0, 4.0],
       [7.0, 5.5], [4.0, 9.0], [9.0, 4.0]],
      3,
      Clusters).
Clusters = [[[1.0, 8.0], [3.0, 7.0], [4.0, 9.0]],
            [[0.5, 1.0], [0.8, 0.5], [0.9, 1.2]]
            [[6.0, 4.0], [7.0, 5.5], [9.0, 4.0]]].
```
