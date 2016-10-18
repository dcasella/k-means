# K-Means / Prolog

## Interface

```prolog
km(+Observations, K, -Clusters)

centroid(+Observations, -Centroid)

vsum(+Vector1, +Vector2, -Vector)

vsub(+Vector1, +Vector2, -Vector)

innerprod(+Vector1, +Vector2, -Value)

norm(+Vector, -Value)

new_vector(+Name, +Vector)
```


## Example

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
