//
// Created by Trystan Maricle on 4/24/22.
//

#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>
#include <string.h>

enum Species {Setosa = 1, Versicolor = 2, Virginica = 3};
typedef struct SQL {
    const char* sepWidthStmt;
    const char* sepLenStmt;
    const char* petLenStmt;
    const char* petWidthStmt;
    sqlite3* db;
    sqlite3_stmt* query;

} Iris;

Iris Statements;

int rowNumbers[];
int species[];
double sepLengths[];
double sepWidths[];
double petLengths[];
double petWidths[];

// read in csv - accepts OPEN file pointer and reads file into buffer for access
void read_db(char* dbFile, int obsSize) {
    if(sqlite3_open(dbFile, &Statements.db) < 0) {
        fprintf(stderr, "Error opening database file. Exiting. \n");
        exit(-1);
    }

    const char* select = "SELECT * FROM Iris;";
    sqlite3_stmt* sqlStmt;
    if(sqlite3_prepare_v2(Statements.db, select, -1, &sqlStmt, NULL ) != SQLITE_OK){
        printf("Error reading sepal length.\n");
        exit(-1);
    }
//    double check = 0;
//    if( sqlite3_bind_double(sqlStmt, 1, rowNum) != SQLITE_OK){
//        printf("Error binding double.\n");
//        exit(-1);
//    }
//    printf("RECEIVED DOUBLE: %f\n", check);
    char* mySpec = (char*) malloc(1024 * sizeof(char));
    for(int i  = 0; i < obsSize; i++){
        int* rowNum = &rowNumbers[i];
        double* sepLen = &sepLengths[i];
        double* sepWid = &sepWidths[i];
        double* petLen = &petLengths[i];
        double* petWid = &petWidths[i];
        int* specID = &species[i];
        int rc = sqlite3_step(sqlStmt);
        if( rc == SQLITE_DONE){
            printf("Finished reading file\n");
            break;
        } else if(rc != SQLITE_ROW){
            printf("Error reading rows.\n");
            exit(-1);
        }
        *rowNum = sqlite3_column_double(sqlStmt, 0);
        *sepLen = sqlite3_column_double(sqlStmt, 1);
        *sepWid = sqlite3_column_double(sqlStmt, 2);
        *petLen = sqlite3_column_double(sqlStmt, 3);
        *petWid = sqlite3_column_double(sqlStmt, 4);
        strcpy(mySpec, (char*)sqlite3_column_text(sqlStmt, 5));
        if(strstr(mySpec, "virginica") != NULL) {
            *specID = Virginica;
        } else if(strstr(mySpec, "setosa") != NULL){
            *specID = Setosa;
        } else if(strstr(mySpec, "versicolor") != NULL){
            *specID = Versicolor;
        }

    }
    free(mySpec);

}

void load_data(double** obs, int obsSize){
    for(int i = 0; i < obsSize; i++){
        obs[i][0] = sepLengths[i];
        obs[i][1] = sepWidths[i];
        obs[i][2] = petLengths[i];
        obs[i][3] = petWidths[i];
        obs[i][4] = species[i];
    }

}

void cleanUp() {
    sqlite3_finalize(Statements.query);
    sqlite3_close(Statements.db);
}

int mainTEST(){
    printf("Reading file.\n");
    read_db("../iris.sqlite", 150);
    int observations_size = 4, rows = 150;
    double** observations = (double **) malloc(sizeof(double *) * observations_size);
    observations = (double **) malloc(sizeof(double *) * observations_size);
    for (int i = 0; i < observations_size; i++)
        observations[i] = (double *) malloc(sizeof(double) * rows);
    printf("Generating Observation Matrix.\n");
    printf("|\tRow Num|\tSepLen|\tSepWid|\tPetLen|\tPetWid|\tSpeciesID|\n");
    for(int i = 0; i < 150; i++) {
        printf("|\t%d|\t%f|\t%f|\t%f|\t%f|\t%d|\n", rowNumbers[i], sepLengths[i], sepWidths[i], petLengths[i], petWidths[i], species[i]);
    }
    fflush(stdout);
    cleanUp();
    return 0;

}