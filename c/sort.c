/* Various sorting algorithms in C
 * Work in progress
 */

#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL -1

/* gets a list of numbers from standard input */
int* get_list_from_input(int size) {
    int *list = (int*) malloc(size*sizeof(int));
    printf("Enter numbers separated by spaces: \n");

    int curr_num;
    for (int i=0; i<size; ++i) {
        scanf(" %d", &curr_num);
        list[i] = curr_num;
    }

    return list;
}

/* prints a list of numbers to standard output */
void print_list(const int* list, int size) {
    for (int i=0; i<size; ++i) {
        printf("%d ", list[i]);
    }
    printf("\n");
}

/* swap elements at indices i and j in list, increments a counter in background */
int swap(int *list, int i, int j, int *num_swaps) {
    if (i != j) {
        int temp = list[i];
        list[i] = list[j];
        list[j] = temp;
        ++(*num_swaps);
    }

    return 0;
}

/* randomise the order of integers in a list */
void shuffle(int *list, int size, int *num_swaps) {
    if (size <= 1) {
        return;
    } else {
        for (int i=0; i<size-1; ++i) {
            int j = i+rand() / (RAND_MAX / (size-i) + 1);
            swap(list, i, j, num_swaps);
        }
    }
}

/* uses the last element in the list as pivot, putting
 * all smaller elements before it, and all larger elements after it
 */
int partition(int* list, int lo, int hi, int *num_swaps) {
    int pivot = list[hi];

    int i = lo;
    for (int j=lo; j<hi; ++j){
        if (list[j] <= pivot) {
            swap(list, i++, j, num_swaps);
        }
    }
    swap(list, i, hi, num_swaps);

    /* return index of where pivot ended up */
    return i;
}

/* sorts a list of numbers */
void quicksort(int *list, int lo, int hi, int *num_swaps) {
    if (lo >= hi) {
        return;
    }

    /* list p is in the right place after partition */
    int p = partition(list, lo, hi, num_swaps);

    quicksort(list, lo, p-1, num_swaps);
    quicksort(list, p+1, hi, num_swaps);
}

/* main procedure */
int main() {
    int size;
    printf("How many numbers do you want to sort? ");
    scanf("%d", &size);

    if (size < 1) {
        printf("Okay, bye!\n");
        return 0;
    }

    int *list = get_list_from_input(size);

    int *num_swaps = (int*) malloc(sizeof(int));
    *num_swaps = 0;
    shuffle(list, size, num_swaps);
    quicksort(list, 0, size-1, num_swaps);
    printf("Quicksort took %d swaps.\n", *num_swaps);

    print_list(list, size);
    return 0;
}
