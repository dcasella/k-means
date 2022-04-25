#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/stat.h>
#include "../include/km.h"
#include "reader.c"
// Example main() with file importing and k-means execution

int main(int argc, char *argv[]) {
    clock_t start, end;
    double cpuTime;
    FILE* fp;
    char* filename;

    int observations_size;
    int vector_size;
    int k;
    double **observations;
    double ***clusters;
    start = clock();
    if(argc == 2 && strstr(argv[1], "iris")){
        filename = "iris.sqlite";
        observations_size = 150;
        vector_size = 5;
        k = 3;
        observations = (double **) malloc(sizeof(double *) * observations_size);
        for (int i = 0; i < observations_size; i++)
            observations[i] = (double *) malloc(sizeof(double) * vector_size);
        struct stat st;
        if (stat(filename, &st) < 0) {
            printf("No such file or directory\n");
            for (int i=0 ; i<observations_size ; ++i) {
                free(observations[i]);
            }
            free(observations);
            exit(-1);
        }
        read_db(filename, observations_size);
        load_data(observations, observations_size);
        printf("Observations:\n");
        print_observations(observations, observations_size, vector_size);
        printf("\n\n");
        start = clock();
        clusters = km(observations, k, observations_size, vector_size);
        end = clock();
        printf("Clusters:\n");
        print_clusters(clusters, k, observations_size, vector_size);
        printf("\n");
        fflush(stdout);
        printf("Freeing allocated memory.\n");
        for (int i=0 ; i<k ; ++i)
            free(clusters[i]);
        free(clusters);

        for (int i=0 ; i<observations_size ; ++i)
            free(observations[i]);
        free(observations);
        cleanUp();
//        fclose(fp);
    } else if (argc > 4) {
        filename = argv[1];
        observations_size = atoi(argv[2]);
        vector_size = atoi(argv[3]);
        k = atoi(argv[4]);
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
            for (int j = 0; j < vector_size; j++) {
                fscanf(fp, "%lf", &observations[i][j]);
            }
        }
        printf("Observations:\n");
        print_observations(observations, observations_size, vector_size);
        printf("\n\n");

        start = clock();
        clusters = km(observations, k, observations_size, vector_size);
        end = clock();
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

    cpuTime = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Start Time: %lu | End Time: %lu | CPU Time: %f\n", start, end, cpuTime);


    return 0;
}