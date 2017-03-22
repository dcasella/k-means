# K-Means > Common Lisp

## Interface

```lisp
(km observations k) → clusters

(centroid observations) → centroid

(vsum vector1 vector2) → vector

(vsub vector1 vector2) → vector

(innerprod vector1 vector2) → value

(norm vector) → value
```


## Example

```lisp
CL prompt> (defparameter v3 (list 1 2 3))
V3

CL prompt> v3
(1 2 3)

CL prompt> (srqt (innerprod V3 V3))
3.7416575

CL prompt> (norm V3)
3.7416575

CL prompt> (vsum V3 (list 10 0 42))
(11 2 45)

CL prompt> (defparameter observations
                         '((3.0 7.0) (0.5 1.0) (0.8 0.5)
                           (1.0 8.0) (0.9 1.2) (6.0 4.0)
                           (7.0 5.5) (4.0 9.0) (9.0 4.0)))

CL prompt> (km observations 3)
(((1.0 8.0) (3.0 7.0) (4.0 9.0))
 ((0.5 1.0) (0.8 0.5) (0.9 1.2))
 ((6.0 4.0) (7.0 5.5) (9.0 4.0)))
```
