#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "point.h"

/* seed the RNG */
void seed() {
    srand(time(NULL));
    for (int i = 0; i < 5; ++i) rand();
}

/* returns a pointer to a new random point in [min, max]^dimension */
struct point* random_point(int dimension, double min, double max) {
    seed();
    struct point *p = malloc(sizeof(struct point));
    p->dim = dimension;
    p->coords = malloc(dimension*sizeof(double));
    for (int i = 0; i < dimension; ++i) {
        rand();   /* throw away one random number */
        double scale = rand() / (double) RAND_MAX;  /* random double between 0 and 1 */
        p->coords[i] = min + scale * (max - min);      /* scale and translate */
    }

    return p;
}

/* print point */
void print_point(struct point *p) {
    if (p == NULL) {
        printf("Received a null point: PRINT_POINT()\n");
        return;
    }
    printf("(");
    for (int i = 0; i < p->dim; ++i) {
        if (i != 0) printf(", ");
        printf("%f", p->coords[i]);
    }
    printf(")\n");
}

/* deletes a point */
void free_point(struct point **p) {
    if (*p == NULL) {
        printf("Received a null point: FREE_POINT()\n");
        return;
    }
    free((*p)->coords);
    free(*p);
    *p = NULL;
}
