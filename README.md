# K-Means

One of the main (and simplier) algorithms used in data statistics analysis is known as the k-means unsupervised clustering algorithm.  
The objective to a clustering algorithm is, given a group of n objects (or observations), partition them into k subsets (or categories) which assemble the objects sharing some properties.  
Particularly, the clustering algorithm k-means partitions n observations into k clusters (groups), where every observation belongs to the group where the centroid is the nearest.  
The Project consists in Common Lisp, Prolog and C libraries implementing Lloyd's k-means algorithm.


___

Lloyd's __k-means algorithm__: pseudo-code
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


__centroid__ _observations_ → _centroid_  


__vsum__ _vector1 vector2_ → _v_  


__vsub__ _vector1 vector2_ → _v_  


__innerprod__ _vector1 vector2_ → _v_  


__norm__ _vector_ → _v_  


### Prolog

__km__(_+Observations_, _+K_, _-Clusters_)  


__centroid__(_+Observations_, _-Centroid_)  


__vsum__(_+Vector1_, _+Vector2_, _-V_)  


__vsub__(_+Vector1_, _+Vector2_, _-V_)  


__innerprod__(_+Vector1_, _+Vector2_, _-R_)  


__norm__(+_Vector_, _-N_)  


__new_vector__(_+Name_, _+Vector_)  


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
