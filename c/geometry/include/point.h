/* a single point in multidimensional space */
struct point {
    int dim;
    double *coords;
};

/* seed the RNG */
void seed();

/* returns a pointer to a new random point in [min, max]^dimension */
struct point* random_point(int dimension, double min, double max);

/* print point */
void print_point(struct point *p);

/* deletes a point */
void free_point(struct point **p);
