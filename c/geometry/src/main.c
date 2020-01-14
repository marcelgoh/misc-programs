/* Implementation of kd-trees */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "point.h"

int main() {
    struct point *p = random_point(4, 0.0, 1.0);
    print_point(p);

    return 0;
}
