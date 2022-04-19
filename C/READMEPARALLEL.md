# K-Means > C > Parralelization

## Interface

```c
double *vsub(const double *vector1, const double *vector2, int vector_size)
double *vsum(const double *vector1, const double *vector2, int vector_size)
int rand_num(int size)
double **initialize(double **observations, int k, int observations_size, int vector_size)
int *partition(double **observations, double **cs, int k, int observations_size, int vector_size)

