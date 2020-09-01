/* Produces a segfault */

#include <stdio.h>
#include <stdlib.h>

int main() {
    int* array;
    array[13] = 3;
    printf("%d\n", array[12]);

    return 0;
}
