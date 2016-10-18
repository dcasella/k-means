# K-Means / Ruby

## Interface

```ruby
km(observations, k) → Clusters

centroid(observations) → Centroid

vsum(vector1, vector2) → Vector

vsub(vector1, vector2) → Vector

innerprod(vector1, vector2) → Value

norm(vector) → Value
```


## Example

```ruby
pry(main)> observations = [[3.0, 7.0], [0.5, 1.0], [0.8, 0.5],
                           [1.0, 8.0], [0.9, 1.2], [6.0, 4.0],
                           [7.0, 5.5], [4.0, 9.0], [9.0, 4.0]]
=> [[3.0, 7.0], [0.5, 1.0], [0.8, 0.5],
    [1.0, 8.0], [0.9, 1.2], [6.0, 4.0],
    [7.0, 5.5], [4.0, 9.0], [9.0, 4.0]]

pry(main)> k = 3
=> 3

pry(main)> km(observations, k)
=> [[[0.5, 1.0], [0.8, 0.5], [0.9, 1.2]],
    [[6.0, 4.0], [7.0, 5.5], [9.0, 4.0]],
    [[3.0, 7.0], [1.0, 8.0], [4.0, 9.0]]]
```
