/* Simulation of the four-is-cosmic game */

#include <stdio.h>
#include <stdlib.h>

/* handle numbers 1-99 */
int double_digit(n) {
    if (n >= 20) {
       /* add tens place and recurse for ones place */
       switch (n / 10) {
           case 4: /* forty */
           case 5: /* fifty */
           case 6: /* sixty */
               return 5 + double_digit(n % 10);
           case 2: /* twenty */
           case 3: /* thirty */
           case 8: /* eighty */
           case 9: /* ninety */
               return 6 + double_digit(n % 10);
           case 7: /* seventy */
               return 7 + double_digit(n % 10);
           default:
               printf("Can't happen.\n");
               return -1;
       }
    } else {
        /* just count letters */
        switch (n) {
            case 1:  /* one */
            case 2:  /* two */
            case 6:  /* six */
            case 10: /* ten */
                return 3;
            case 4:  /* four -- cosmic! */
            case 5:  /* five */
            case 9:  /* nine */
                return 4;
            case 3:  /* three */
            case 7:  /* seven */
            case 8:  /* eight */
                return 5;
            case 11: /* eleven */
            case 12: /* twelve */
                return 6;
            case 15: /* fifteen */
            case 16: /* sixteen */
                return 7;
            case 13: /* thirteen */
            case 14: /* fourteen */
            case 18: /* eighteen */
            case 19: /* nineteen */
                return 8;
            case 17: /* seventeen */
                return 9;
            case 0:  /* EMPTY PLACE VALUE */
                return 0;
            default:
                printf("Can't happen.\n");
                return -1;
        }
    }
}

/* recursively count number of letters */
int num_letters(n) {
    if (n >= 1000000) {
        /* "... million ..." */
        return num_letters(n / 1000000) + 7 + num_letters(n % 1000000);
    }
    if (n >= 1000) {
        /* "... thousand ..." */
        return num_letters(n / 1000) + 8 + num_letters(n % 1000);
    }
    if (n >= 100) {
        /* "... hundred ..." */
        return num_letters(n / 100) + 7 + num_letters(n % 100);
    }
    return double_digit(n);
}

int print_and_count(n) {
    if (n == 4) {
        printf("4 is cosmic.\n");
        return 1;
    }

    int next = num_letters(n);

    printf("%d is %d.\n", n, next);
    return 1 + print_and_count(next);
}

int just_count(n) {
    if (n == 4) {
        return 1;
    }
    return 1 + just_count(num_letters(n));
}

int main(int argc, char *argv[]) {
    int number;
    int count;
    if (argc == 1) {
        printf("Enter a natural number up to 999999999: ");
        scanf("%d", &number);

        if (number < 1 || number > 999999999) {
            printf("Invalid number.\n");
            return -1;
        }

        count = print_and_count(number);
        printf("The sequence has %d elements.\n", count);
    } else {
        if (sscanf(argv[1], "%i", &number) !=  1) {
            printf("Failed to parse integer.\n");
            return -1;
        }
        /* ensure enough space for jumping forward (for cases 1,2,3) */
        int arrlen = (number > 5) ? number : 5;
        int *arr = (int*) malloc(arrlen*sizeof(int));
        for (int i=1; i<=number; ++i) {
            count = just_count(i);
            arr[i - 1] = count;
        }

        int max_index = -1;
        int max_value = 0;

        for (int i=1; i<=number; ++i) {
            if (arr[i - 1] > max_value) {
                max_value = arr[i - 1];
                max_index = i;
            }
        }

        printf("%d has the longest sequence (%d elements).\n", max_index, max_value);
    }

    return 0;
}
