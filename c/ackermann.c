#include <stdio.h>
#include <stdlib.h>

/* Written by Marcel Goh on 12 May 2018. This program tries to compute the Ackermann function up to 
 * ack(5,5). The program will segfault trying to compute ack(4,2). 
 */

long int count = 0;
int ack(int x, int y) {
    ++count;
    if (x < 0 || y < 0) {
        return -1;
    } else if (x == 0) {
        return y + 1;
    } else if (y == 0) {
        return ack(x - 1, 1);
    } else {
        return ack(x - 1, ack(x, y - 1));
    }
}

int main() {
    for (int i=0; i<6; ++i) {
        for (int j=0; j<6; ++j) {
            printf("ackermann(%d,%d) = %5d. %10ld steps.\n", i, j, ack(i,j), count);
        }
    }
    return 0;
}
