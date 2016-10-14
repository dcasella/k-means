#include "../include/km.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include <time.h>
#define ERR_NO_NUM -1
#define ERR_NO_MEM -2


int *clusters_sizes;

void print_vector(double *vector, int vector_size) {
	printf("(");

	for (int i = 0; i < vector_size; i++) {
		if (i > 0)
			printf(", ");

		printf("%.2f", vector[i]);
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

void print_clusters(double ***clusters, int k, int observations_size, int vector_size) {
	printf("{");

	for (int i = 0; i < k; i++) {
		if (i > 0)
			printf(", ");

		print_observations(clusters[i], clusters_sizes[i], vector_size);
	}

	free(clusters_sizes);

	printf("}");
}

int compare_clusters(const int *clusters_map1, const int *clusters_map2, int clusters_size) {
	for (int i = 0; i < clusters_size; i++) {
		if (clusters_map1[i] != clusters_map2[i])
			return 0;
	}

	return 1;
}

double ***km(double **observations, int k, int observations_size, int vector_size) {
	clusters_sizes = (int *) malloc(sizeof(int) * k);
	int *clusters_map = (int *) malloc(sizeof(int) * observations_size);
	int *new_clusters_map = (int *) malloc(sizeof(int) * observations_size);
	double **cs = (double **) malloc(sizeof(double *) * k);
	/*
	 * Why initialize every cs[i] when at #74 you assign something to cs?
	for (int i = 0; i < k; i++)
		cs[i] = (double *) malloc(sizeof(double) * vector_size);
	*/
	cs = initialize(observations, k, observations_size, vector_size);

	if (observations_size < k) {
		printf("Can't compute clusters.");
		free(clusters_sizes);
		free(clusters_map);
		free(new_clusters_map);
		free(cs);
		/* Just in case you think about freeing every cs[i]... DON'T.
		 * cs[i] points to a vector in observations */
		exit(1);
	}

	while (1) {
		new_clusters_map = partition(observations, cs, k, observations_size, vector_size);

		if (compare_clusters(clusters_map, new_clusters_map, observations_size)) {
			free(new_clusters_map);
			free(cs);

			return map_clusters(clusters_map, observations, k, observations_size, vector_size);
		}
		/* WHY MEMCPY? "sei tardo?!?1?!"
		 * also remember to free old cluster map, you don't need them
		memcpy(clusters_map, new_clusters_map, sizeof(int) * observations_size);
		*/
		free(clusters_map);
		cluster_maps = new_clusters_map;
		cs = re_centroids(clusters_map, observations, k, observations_size, vector_size);
	}
}

double *centroid(double **observations, int observations_size, int vector_size) {
	double *vector = (double *) calloc(vector_size, sizeof(double));

	for (int i = 0; i < observations_size; i++)
		vector = vsum(vector, observations[i], vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] /= observations_size;

	return vector;
}

double *vsum(const double *vector1, const double *vector2, int vector_size) {
	double *vector = (double *) malloc(sizeof(double) * vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] + vector2[i];

	return vector;
}

double *vsub(const double *vector1, const double *vector2, int vector_size) {
	double *vector = (double *) malloc(sizeof(double) * vector_size);

	for (int i = 0; i < vector_size; i++)
		vector[i] = vector1[i] - vector2[i];

	return vector;
}

double innerprod(const double *vector1, const double *vector2, int vector_size) {
	double prod = 0;

	for (int i = 0; i < vector_size; i++)
		prod += vector1[i] *vector2[i];

	return prod;
}

double norm(const double *vector, int vector_size) {
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

double **initialize(double **observations, int k, int observations_size, int vector_size) {
	double **centroids = (double **) malloc(sizeof(double *) * k);
	/* Just in case you think about removing also this malloc after seeing my changes - DON'T
	 * See #194, this is exactly when this is needed */
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

int *partition(double **observations, double **cs, int k, int observations_size, int vector_size) {
	int *clusters_map = (int *) malloc(sizeof(int) * observations_size);
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

double **re_centroids(int *clusters_map, double **observations, int k, int observations_size, int vector_size) {
	double **centroids = (double **) malloc(sizeof(double *) * k);
	/* No need to allocate space that will not be written
	 * centroids[i] will point somewhere at #246
	for (int i = 0; i < k; i++)
		centroids[i] = (double *) malloc(sizeof(double) * vector_size);
	*/
	double **temp_arr = (double **) malloc(sizeof(double *) * observations_size);
	/* Same as above, see #241
	for (int i = 0; i < observations_size; i++)
		temp_arr[i] = (double *) malloc(sizeof(double) * vector_size);
	*/
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

double ***map_clusters(int *clusters_map, double **observations, int k, int observations_size, int vector_size) {
	double ***clusters = (double ***) malloc(sizeof(double **) * k);
	
	/* for some reasons here you didn't thought about malloc-ing everything, nice */

	for (int i = 0; i < k; i++)
		clusters[i] = map_cluster(clusters_map, observations, i, observations_size, vector_size);

	return clusters;
}

double **map_cluster(const int *clusters_map, double **observations, int c, int observations_size, int vector_size) {
	int count = 0;
	int *temp_arr = (int *) malloc(sizeof(int) * observations_size);

	for (int i = 0; i < observations_size; i++) {
		if (clusters_map[i] == c) {
			temp_arr[count] = i;
			count++;
		}
	}

	double **cluster = (double **) malloc(sizeof(double *) * count);
	/* 1)
	 * 	i from 0 to observation_size
	 * 	but #277 allocates a number of "double *" equal to "count"
	 * 2)
	 *	again, why allocate space that will be overwritten at #287 ?
	for (int i = 0; i < observations_size; i++)
		cluster[i] = (double *) malloc(sizeof(double) * vector_size);
	*/
	for (int i = 0; i < count; i++)
		cluster[i] = observations[temp_arr[i]];

	free(temp_arr);
	clusters_sizes[c] = count;

	return cluster;
}
