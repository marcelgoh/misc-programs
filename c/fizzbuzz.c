/* Fizzbuzz problem */

#include <stdio.h>

int main() {
    int x, y, n;
    scanf("%d %d %d", &x, &y, &n);

    for (int i=1; i<=n; ++i) {
        int found = 0;
        if (i % x == 0) {
            printf("Fizz");
            found = 1;
        }
        if (i % y == 0) {
            printf("Buzz");
            found = 1;
        }
        if (found == 0) {
            printf("%d", i);
        }
        printf("\n");
    }

    return 0;
}
