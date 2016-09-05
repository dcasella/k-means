#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#define ERR_NO_NUM -1
#define ERR_NO_MEM -2

void print_vector(double* vector, size_t vector_size);
void print_observations(double** observations, size_t observations_size, size_t vector_size);
void print_clusters(int* clusters, double** observations, int k, size_t observations_size, size_t vector_size);
int compare_clusters(int* cluster1, int* cluster2, size_t clusters_size);

int *km(double**, int, size_t, size_t);
double *centroid(double**, size_t, size_t);
double *vsum(double*, double*, size_t);
double *vsub(double*, double*, size_t);
double innerprod(double*, double*, size_t);
double norm(double*, size_t);

int random(int);

double **initialize(double**, int, size_t, size_t);
int *partition(double**, double**, size_t, size_t, size_t);
double **re_centroids(int*, double**, size_t, size_t, size_t);

int main() {
	int k = 3;
	int vector_size = 2;
	int observations_size = 9;
	FILE *fp;

	double **observations = malloc(sizeof(double*) * observations_size);
	for (int i = 0; i < observations_size; i++)
		observations[i] = malloc(sizeof(double) * vector_size);

	if ((fp = fopen("C:/Users/Cryoken/Downloads/ant42_9.txt", "r+")) == NULL) {
		printf("File doesn't exist.");
		exit(1);
	}

	for (int i = 0; i < observations_size; i++)
		for (int j = 0; j < vector_size; j++)
			fscanf(fp, "%lf", &observations[i][j]);

	printf("OBSERVATIONS:\n");
	print_observations(observations, observations_size, vector_size);
	printf("\n\n");

	int *clusters = km(observations, k, observations_size, vector_size);
	printf("CLUSTERS:\n");
	print_clusters(clusters, observations, k, observations_size, vector_size);

	getchar();
}

void print_vector(double *vector, size_t vector_size) {
	printf("(");

	for (size_t i = 0; i < vector_size; i++) {
		if (i > 0)
			printf(", ");

		printf("%f", vector[i]);
	}

	printf(")");
}

void print_observations(double **observations, size_t observations_size, size_t vector_size) {
	printf("[");

	for (size_t i = 0; i < observations_size; i++) {
		if (i > 0)
			printf(", ");

		print_vector(observations[i], vector_size);
	}

	printf("]");
}

void print_clusters(int *clusters, double **observations, int k, size_t observations_size, size_t vector_size) {
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

int compare_clusters(int *clusters1, int *clusters2, size_t clusters_size) {
	for (size_t i = 0; i < clusters_size; i++) {
		if (clusters1[i] != clusters2[i])
			return 0;
	}

	return 1;
}

int *km(double **observations, int k, size_t observations_size, size_t vector_size) {
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
}

double *centroid(double **observations, size_t observations_size, size_t vector_size) {
	double *vector = calloc(vector_size, sizeof(double));

	for (size_t i = 0; i < observations_size; i++)
		vector = vsum(vector, observations[i], vector_size);
	
	for (size_t i = 0; i < vector_size; i++)
		vector[i] /= observations_size;

	return vector;
}

double *vsum(double *vector1, double *vector2, size_t vector_size) {
	double *vector = malloc(vector_size * sizeof(double));

	for (size_t i = 0; i < vector_size; i++)
		vector[i] = vector1[i] + vector2[i];

	return vector;
}

double *vsub(double *vector1, double *vector2, size_t vector_size) {
	double *vector = malloc(vector_size * sizeof(double));

	for (size_t i = 0; i < vector_size; i++)
		vector[i] = vector1[i] - vector2[i];

	return vector;
}

double innerprod(double *vector1, double *vector2, size_t vector_size) {
	double prod = 0;

	for (size_t i = 0; i < vector_size; i++)
		prod += vector1[i] * vector2[i];

	return prod;
}

double norm(double *vector, size_t vector_size) {
	return sqrt(innerprod(vector, vector, vector_size));
}

int random(int size) {
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

double **initialize(double **observations, int k, size_t observations_size, size_t vector_size) {
	double **centroids = malloc(sizeof(double*) * k);
	for (int i = 0; i < k; i++)
		centroids[i] = malloc(sizeof(double) * vector_size);

	srand(time(NULL));
	int r = random(observations_size);

	for (int i = 0; i < k; i++) {
		for (int j = 0; j < vector_size; j++) {
			centroids[i][j] = observations[r][j];
		}

		r = random(-1);
	}

	return centroids;
}

int *partition(double **observations, double **cs, size_t k, size_t observations_size, size_t vector_size) {
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

double **re_centroids(int *clusters, double **observations, size_t k, size_t observations_size, size_t vector_size) {
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
