#include <stdio.h>
#include <stdlib.h>
#include "include/km.h"


// Example main() with file importing and k-means execution

int main(int argc, char *argv[]) {
    if (argc > 4) {
        FILE *fp;
        char *filename = argv[1];
        /* Make sure you update observations_size, vector_size and k
         * accordingly to your needs
         */
        int observations_size = atoi(argv[2]);
        int vector_size = atoi(argv[3]);
        int k = atoi(argv[4]);
        double **observations;
        double ***clusters;
        
        observations = (double **) malloc(sizeof(double *) * observations_size);
        for (int i = 0; i < observations_size; i++)
            observations[i] = (double *) malloc(sizeof(double) * vector_size);
        
        if ((fp = fopen(filename, "r+")) == NULL) {
            printf("No such file or directory\n");
            for (int i=0 ; i<observations_size ; ++i)
                free(observations[i]);
            free(observations);
            exit(1);
        }
        
        for (int i = 0; i < observations_size; i++) {
            for (int j = 0; j < vector_size; j++)
                fscanf(fp, "%lf", &observations[i][j]);
        }
        
        printf("Observations:\n");
        print_observations(observations, observations_size, vector_size);
        printf("\n\n");
        
        clusters = km(observations, k, observations_size, vector_size);
        printf("Clusters:\n");
        print_clusters(clusters, k, observations_size, vector_size);
        printf("\n");
        
        for (int i=0 ; i<k ; ++i)
            free(clusters[i]);
        free(clusters);
        
        for (int i=0 ; i<observations_size ; ++i)
            free(observations[i]);
        free(observations);
        fclose(fp);
    }
    
    return 0;
}
