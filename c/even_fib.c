/* Sums the even Fibonacci numbers up to a threshold */

#include <stdio.h>

int main() {
    printf("Enter threshold: ");
    int max;
    scanf("%d", &max);

    int a = 1;
    int b = 1;

    int acc = 0;
    while (1) {
        int sum = a + b;
        if (sum > max) {
            break;
        }
        if (sum % 2 == 0) {
            acc += sum;
        }
        a = b;
        b = sum;
    }
    printf("The total is %d.\n", acc);

    return 0;
}
