/* Various sorting algorithms in C */

#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL -1

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

void print_list(const int* list, int size) {
    for (int i=0; i<size; ++i) {
        printf("%d ", list[i]);
    }
    printf("\n");
}

int main() {
    int size;
    printf("How many numbers do you want to sort? ");
    scanf("%d", &size);

    if (size < 1) {
        printf("Okay, bye!\n");
        return 0;
    }

    int *list = get_list_from_input(size);

    print_list(list, size);
    return 0;
}
