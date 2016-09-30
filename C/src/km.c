#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <time.h>
#define ERR_NO_NUM -1
#define ERR_NO_MEM -2

// Declaration
void print_vector(double * vector, int vector_size);
void print_observations(double ** observations, int observations_size, int vector_size);
void print_clusters(double *** clusters, int k, int observations_size, int vector_size);
int compare_clusters(int * cluster_map1, int * cluster_map2, int clusters_size);

double *** km(double ** observations, int k, int observations_size, int vector_size);
double * centroid(double ** observations, int observations_size, int vector_size);
double * vsum(double * vector1, double * vector2, int vector_size);
double * vsub(double * vector1, double * vector2, int vector_size);
double innerprod(double * vector1, double * vector2, int vector_size);
double norm(double * vector, int vector_size);

int rand_num(int size);

double ** initialize(double ** observations, int k, int observations_size, int vector_size);
int * partition(double ** observations, double ** cs, int k, int observations_size, int vector_size);
double ** re_centroids(int * clusters_map, double ** observations, int k, int observations_size, int vector_size);
double *** map_clusters(int * clusters_map, double ** observations, int k, int observations_size, int vector_size);
double ** map_cluster(int * clusters_map, double ** observations, int cluster, int observations_size, int vector_size);

int * clusters_sizes;

// Implementation
void print_vector(double * vector, int vector_size) {
	printf("(");

	for (int i = 0; i < vector_size; i++) {
		if (i > 0)
			printf(", ");

		//printf("%.2f", vector[i]);  // to print only 2 decimal values
		printf("%f", vector[i]);
	}

	printf(")");
}

void print_observations(double ** observations, int observations_size, int vector_size) {
	printf("[");

	for (int i = 0; i < observations_size; i++) {
		if (i > 0)
			printf(", ");

		print_vector(observations[i], vector_size);
	}

	printf("]");
}

void print_clusters(double *** clusters, int k, int observations_size, int vector_size) {
	printf("{");

	for (int i = 0; i < k; i++) {
		if (i > 0)
			printf(", ");

		print_observations(clusters[i], clusters_sizes[i], vector_size);
	}

	printf("}");
}

int compare_clusters(int * clusters_map1, int * clusters_map2, int clusters_size) {
	for (int i = 0; i < clusters_size; i++) {
		if (clusters_map1[i] != clusters_map2[i])
			return 0;
	}

	return 1;
}

double *** km(double ** observations, int k, int observations_size, int vector_size) {
	clusters_sizes = (int *) malloc(sizeof(int) * k);
	int * clusters_map = (int *) malloc(sizeof(int) * observations_size);
	int * new_clusters_map = (int *) malloc(sizeof(int) * observations_size);
	double ** cs = (double **) malloc(sizeof(double *) * k);
	for (int i = 0; i < k; i++)
		cs[i] = (double *) malloc(sizeof(double) * vector_size);

	cs = initialize(observations, k, observations_size, vector_size);

	if (observations_size < k) {
		printf("Can't compute clusters.");
		exit(1);
	}

	while (1) {
		new_clusters_map = partition(observations, cs, k, observations_size, vector_size);

		if (compare_clusters(clusters_map, new_clusters_map, observations_size)) {
			free(new_clusters_map);
			free(cs);

			return map_clusters(clusters_map, observations, k, observations_size, vector_size);
		}

		memcpy(clusters_map, new_clusters_map, sizeof(int) * observations_size);
		cs = re_centroids(new_clusters_map, observations, k, observations_size, vector_size);
	}
}

double * centroid(double ** observations, int observations_size, int vector_size) {
	double * vector = (double *) calloc(vector_size, sizeof(double));

	for (int i = 0; i < observations_size; i++)
		vector = vsum(vector, observations[i], vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] /= observations_size;

	return vector;
}

double * vsum(double * vector1, double * vector2, int vector_size) {
	double * vector = (double *) malloc(sizeof(double) * vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] + vector2[i];

	return vector;
}

double * vsub(double * vector1, double * vector2, int vector_size) {
	double * vector = (double *) malloc(sizeof(double) * vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] - vector2[i];

	return vector;
}

double innerprod(double * vector1, double * vector2, int vector_size) {
	double prod = 0;

	for (int i = 0; i < vector_size; i++)
		prod += vector1[i] * vector2[i];

	return prod;
}

double norm(double * vector, int vector_size) {
	return sqrt(innerprod(vector, vector, vector_size));
}

/* Loved this shuffling random algorithm
 * Source: http://stackoverflow.com/a/5064432
 */
int rand_num(int size) {
	int i, n;
	static int numNums = 0;
	static int * numArr = NULL;

	if (size >= 0) {
		if (numArr != NULL)
			free(numArr);
		if ((numArr = (int *) malloc(sizeof(int) * size)) == NULL)
			return ERR_NO_MEM;
		for (i = 0; i < size; i++)
			numArr[i] = i;
		numNums = size;
	}

	if (numNums == 0)
		return ERR_NO_NUM;

	n = rand() % numNums;
	i = numArr[n];
	numArr[n] = numArr[numNums - 1];
	numNums--;
	if (numNums == 0) {
		free(numArr);
		numArr = 0;
	}

	return i;
}

double ** initialize(double ** observations, int k, int observations_size, int vector_size) {
	double ** centroids = (double **) malloc(sizeof(double *) * k);
	for (int i = 0; i < k; i++)
		centroids[i] = (double *) malloc(sizeof(double) * vector_size);

	srand(time(NULL));
	int r = rand_num(observations_size);

	for (int i = 0; i < k; i++) {
		for (int j = 0; j < vector_size; j++) {
			centroids[i][j] = observations[r][j];
		}

		r = rand_num(-1);
	}

	return centroids;
}

int * partition(double ** observations, double ** cs, int k, int observations_size, int vector_size) {
	int * clusters_map = (int *) malloc(sizeof(int) * observations_size);
	float curr_distance;
	int centroid;

	for (int i = 0; i < observations_size; i++) {
		float min_distance = DBL_MAX;

		for (int c = 0; c < k; c++) {
			if ((curr_distance = norm(vsub(observations[i], cs[c], vector_size), vector_size)) < min_distance) {
				min_distance = curr_distance;
				centroid = c;
			}
		}

		clusters_map[i] = centroid;
	}

	return clusters_map;
}

double ** re_centroids(int * clusters_map, double ** observations, int k, int observations_size, int vector_size) {
	double ** centroids = (double **) malloc(sizeof(double *) * k);
	for (int i = 0; i < k; i++)
		centroids[i] = (double *) malloc(sizeof(double) * vector_size);

	double ** temp_arr = (double **) malloc(sizeof(double *) * observations_size);
	for (int i = 0; i < observations_size; i++)
		temp_arr[i] = (double *) malloc(sizeof(double) * vector_size);

	for (int c = 0, count = 0; c < k; c++) {
		for (int i = 0; i < observations_size; i++) {
			int curr = clusters_map[i];

			if (curr == c) {
				temp_arr[count] = observations[i];
				count++;
			}
		}

		centroids[c] = centroid(temp_arr, count, vector_size);
		count = 0;
	}

	free(temp_arr);

	return centroids;
}

double *** map_clusters(int * clusters_map, double ** observations, int k, int observations_size, int vector_size) {
	double *** clusters = (double ***) malloc(sizeof(double **) * k);

	for (int i = 0; i < k; i++)
		clusters[i] = map_cluster(clusters_map, observations, i, observations_size, vector_size);

	return clusters;
}

double ** map_cluster(int * clusters_map, double ** observations, int cluster, int observations_size, int vector_size) {
	int count = 0;
	int * temp_arr = (int *) malloc(sizeof(int *) * observations_size);

	for (int i = 0; i < observations_size; i++) {
		if (clusters_map[i] == cluster) {
			temp_arr[count] = i;
			count++;
		}
	}

	double ** clusters = (double **) malloc(sizeof(double *) * count);
	for (int i = 0; i < observations_size; i++)
		clusters[i] = (double *) malloc(sizeof(double) * vector_size);

	for (int i = 0; i < count; i++)
		clusters[i] = observations[temp_arr[i]];

	free(temp_arr);
	clusters_sizes[cluster] = count;

	return clusters;
}
