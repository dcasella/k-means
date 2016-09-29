#ifndef KM_H
#define KM_H

void print_vector(double * vector, int vector_size);
void print_observations(double ** observations, int observations_size, int vector_size);
void print_clusters(int * clusters, double ** observations, int k, int observations_size, int vector_size);
int compare_clusters(int * cluster1, int * cluster2, int clusters_size);

int * km(double ** observations, int k, int observations_size, int vector_size);
double * centroid(double ** observations, int observations_size, int vector_size);
double * vsum(double * vector1, double * vector2, int vector_size);
double * vsub(double * vector1, double * vector2, int vector_size);
double innerprod(double * vector1, double * vector2, int vector_size);
double norm(double * vector, int vector_size);

int rand_num(int size);

double ** initialize(double ** observations, int k, int observations_size, int vector_size);
int * partition(double ** observations, double ** cs, int k, int observations_size, int vector_size);
double ** re_centroids(int * clusters, double ** observations, int k, int observations_size, int vector_size);

#endif
