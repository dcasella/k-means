#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include "km.h"

int main() {
	int k = 3;
	int vector_size = 2;
	int observations_size = 9;
	FILE *fp;

	double **observations = malloc(sizeof(double*) * observations_size);
	for (int i = 0; i < observations_size; i++)
		observations[i] = malloc(sizeof(double) * vector_size);

	if ((fp = fopen("Test_9.txt", "r+")) == NULL) {
		printf("File doesn't exist.\n");
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
	printf("\n");
}
