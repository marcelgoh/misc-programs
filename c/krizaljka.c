/* Krizaljka: Create a two-word crossword
 * From Kattis.com, written by Marcel Goh on 11 October 2018
 */

#include <stdio.h>
#include <string.h>

int main() {
    char a[64];
    char b[64];
    scanf("%s %s", a, b);

    int n = strlen(a);
    int m = strlen(b);

    int a_i, b_j;

    int found = 0;

    for (int i=0; i<n; ++i) {
        for (int j=0; j<m; ++j) {
            if (a[i] == b[j]) {
                a_i = i;
                b_j = j;
                found = 1;
                break;
            }
        }
        if (found == 1) {
            break;
        }
    }

    for (int i=0; i<m; ++i) {
        if (i == b_j) {
            printf("%s\n", a);
        } else {
            for (int j=0; j<n; ++j) {
                if (j == a_i) {
                    printf("%c", b[i]);
                } else {
                    printf("%c", '.');
                }
                if (j == n-1) {
                    printf("\n");
                }
            }
        }
    }

    return 0;
}
