#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <time.h>
#define ERR_NO_NUM -1
#define ERR_NO_MEM -2

// Declaration
void print_vector(double *vector, int vector_size);
void print_observations(double **observations, int observations_size, int vector_size);
void print_clusters(int *clusters, double **observations, int k, int observations_size, int vector_size);
int compare_clusters(int *cluster1, int *cluster2, int clusters_size);

int *km(double **observations, int k, int observations_size, int vector_size);
double *centroid(double **observations, int observations_size, int vector_size);
double *vsum(double *vector1, double *vector2, int vector_size);
double *vsub(double *vector1, double *vector2, int vector_size);
double innerprod(double *vector1, double *vector2, int vector_size);
double norm(double *vector, int vector_size);

int rand_num(int size);

double **initialize(double **observations, int k, int observations_size, int vector_size);
int *partition(double **observations, double **cs, int k, int observations_size, int vector_size);
double **re_centroids(int *clisters, double **observations, int k, int observations_size, int vector_size);

// Implementation
void print_vector(double *vector, int vector_size) {
	printf("(");

	for (int i = 0; i < vector_size; i++) {
		if (i > 0)
			printf(", ");

		printf("%f", vector[i]);
	}

	printf(")");
}

void print_observations(double **observations, int observations_size, int vector_size) {
	printf("[");

	for (int i = 0; i < observations_size; i++) {
		if (i > 0)
			printf(", ");

		print_vector(observations[i], vector_size);
	}

	printf("]");
}

void print_clusters(int *clusters, double **observations, int k, int observations_size, int vector_size) {
	double **temp_arr = malloc(sizeof(double*) * observations_size);
	for (int i = 0; i < observations_size; i++)
		temp_arr[i] = malloc(sizeof(double) * vector_size);

	printf("{");

	for (int c = 0, count = 0; c < k; c++) {
		for (int i = 0; i < observations_size; i++) {
			int curr = clusters[i];

			if (curr == c) {
				temp_arr[count] = observations[i];
				count++;
			}
		}

		if (c > 0)
			printf(", ");

		print_observations(temp_arr, count, vector_size);
		count = 0;
	}

	free(temp_arr);

	printf("}");
}

int compare_clusters(int *clusters1, int *clusters2, int clusters_size) {
	for (int i = 0; i < clusters_size; i++) {
		if (clusters1[i] != clusters2[i])
			return 0;
	}

	return 1;
}

int *km(double **observations, int k, int observations_size, int vector_size) {
	int tumore_al_cervello = 1;
	int *clusters = malloc(observations_size * sizeof(int));
	int *new_clusters = malloc(observations_size * sizeof(int));
	double **cs = malloc(sizeof(double*) * k);
	for (int i = 0; i < k; i++)
		cs[i] = malloc(sizeof(double) * vector_size);

	cs = initialize(observations, k, observations_size, vector_size);

	if (observations_size < k) {
		printf("Can't compute clusters.");
		tumore_al_cervello = 0;
		exit(1);
	}

	while (tumore_al_cervello == 1) {
		new_clusters = partition(observations, cs, k, observations_size, vector_size);

		if (compare_clusters(clusters, new_clusters, observations_size)) {
			free(new_clusters);
			free(cs);

			return clusters;
		}

		memcpy(clusters, new_clusters, observations_size * sizeof(int));
		cs = re_centroids(new_clusters, observations, k, observations_size, vector_size);
	}

	return clusters; // Just for fun
}

double *centroid(double **observations, int observations_size, int vector_size) {
	double *vector = calloc(vector_size, sizeof(double));

	for (int i = 0; i < observations_size; i++)
		vector = vsum(vector, observations[i], vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] /= observations_size;

	return vector;
}

double *vsum(double *vector1, double *vector2, int vector_size) {
	double *vector = malloc(vector_size * sizeof(double));

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] + vector2[i];

	return vector;
}

double *vsub(double *vector1, double *vector2, int vector_size) {
	double *vector = malloc(vector_size * sizeof(double));

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] - vector2[i];

	return vector;
}

double innerprod(double *vector1, double *vector2, int vector_size) {
	double prod = 0;

	for (int i = 0; i < vector_size; i++)
		prod += vector1[i] * vector2[i];

	return prod;
}

double norm(double *vector, int vector_size) {
	return sqrt(innerprod(vector, vector, vector_size));
}

/* Loved this shuffling random algorithm
 * Source: http://stackoverflow.com/a/5064432
 */
int rand_num(int size) {
	int i, n;
	static int numNums = 0;
	static int *numArr = NULL;

	if (size >= 0) {
		if (numArr != NULL)
			free(numArr);
		if ((numArr = malloc(sizeof(int) * size)) == NULL)
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

double **initialize(double **observations, int k, int observations_size, int vector_size) {
	double **centroids = malloc(sizeof(double*) * k);
	for (int i = 0; i < k; i++)
		centroids[i] = malloc(sizeof(double) * vector_size);

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

int *partition(double **observations, double **cs, int k, int observations_size, int vector_size) {
	int *clusters = malloc(observations_size * sizeof(int));
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

		clusters[i] = centroid;
	}

	return clusters;
}

double **re_centroids(int *clusters, double **observations, int k, int observations_size, int vector_size) {
	double **centroids = malloc(sizeof(double*) * k);
	for (int i = 0; i < k; i++)
		centroids[i] = malloc(sizeof(double) * vector_size);

	double **temp_arr = malloc(sizeof(double*) * observations_size);
	for (int i = 0; i < observations_size; i++)
		temp_arr[i] = malloc(sizeof(double) * vector_size);

	for (int c = 0, count = 0; c < k; c++) {
		for (int i = 0; i < observations_size; i++) {
			int curr = clusters[i];

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
