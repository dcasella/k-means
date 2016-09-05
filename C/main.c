#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include "km.h"

// Example main() with file importing and k-means execution

int main() {
	FILE *fp;
	// Initialization
	char *filename = "Test_9.txt";
	int observations_size = 9;
	int vector_size = 2;
	int k = 3;
	/* Make sure you update observations_size, vector_size and k
	 * accordingly to your needs
	 */

	double **observations = malloc(sizeof(double*) * observations_size);
	for (int i = 0; i < observations_size; i++)
		observations[i] = malloc(sizeof(double) * vector_size);

	if ((fp = fopen(filename, "r+")) == NULL) {
		printf("File doesn't exist.\n");
		exit(1);
	}

	for (int i = 0; i < observations_size; i++)
		for (int j = 0; j < vector_size; j++)
			fscanf(fp, "%lf", &observations[i][j]);

	printf("Observations:\n");
	print_observations(observations, observations_size, vector_size);
	printf("\n\n");

	int *clusters = km(observations, k, observations_size, vector_size);
	printf("Clusters:\n");
	print_clusters(clusters, observations, k, observations_size, vector_size);
	printf("\n");

	getchar(); // Keep the terminal open (if Windows cmd)
}
