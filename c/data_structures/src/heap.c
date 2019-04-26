/* Min-heap data structure (implemented with arrays)
 * Last updated 26 April 2019 by Marcel Goh
 */

#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "dynarray.h" // use resizing array representation

#define ERR_VAL INT_MIN

typedef struct heap_s {
    DYNARRAY *array;
    int size;
} HEAP;

HEAP* new_heap() {
    HEAP *new = malloc(sizeof(HEAP));
    new->array = new_da(10);
    new->size = 0;
    return new;
}

int free_heap(HEAP **heap) {
    if (*heap == NULL) {
        printf("Passed a NULL heap: HEAP.FREE_HEAP()");
        return ERR_VAL;
    }
    free_da(&((*heap)->array));
    free(*heap);
    *heap = NULL;

    return 0;
}

int add_heap(HEAP *heap, int key, char value) {
    if (heap == NULL) {
        printf("Passed a NULL heap: HEAP.ADD_HEAP()");
        return ERR_VAL;
    }
    int idx = heap->size;
    DYNARRAY *arr = heap->array;
    /* add element at bottom of heap */
    add_da(arr, key, value);
    ++(heap->size);
    /* swim the element as high as needed */
    while (idx != 0) {
        int parent = (idx - 1) / 2;
        if (compare_item(get_da(arr, idx), get_da(arr, parent)) < 0) {
            swap_da(arr, idx, parent);
        } else {
            break;
        }
        idx = parent;
    }

    return 0;
}

ITEM* peek_min(const HEAP *heap) {
    if (heap == NULL) {
        printf("Passed a NULL heap: HEAP.PEEK_MIN()");
        return NULL;
    }
    if (heap->size == 0) {
        printf("Cannot peek at empty heap: HEAP.PEEK_MIN()");
        return NULL;
    }
    return get_da(heap->array, 0);
}

ITEM* remove_min(HEAP *heap) {
    if (heap == NULL) {
        printf("Passed a NULL heap: HEAP.REMOVE_MIN()");
        return NULL;
    }
    if (heap->size == 0) {
        printf("Cannot remove from empty heap: HEAP.REMOVE_MIN()");
        return NULL;
    }

    DYNARRAY *arr = heap->array;
    /* copy bottom to top of heap, then delete bottom */
    ITEM *min = get_da(arr, 0);
    ITEM *bottom = get_da(arr, heap->size - 1);
    set_da(arr, 0, bottom->key, bottom->value);
    remove_da(arr, heap->size - 1);
    --(heap->size);

    /* sink element to proper position */
    int parent = 0;
    while (2 * parent + 1 < heap->size) { // is there a left child?
        int child = 2 * parent + 1;
        if (child + 1 < heap->size) { // is there a right child?
            if (compare_item(get_da(arr, child), get_da(arr, child + 1)) > 0) {
                /* if right child is smaller, we choose right branch */
                ++child;
            }
        }

        /* if child is smaller, sink parent down one level */
        if (compare_item(get_da(arr, parent), get_da(arr, child)) > 0) {
            swap_da(arr, parent, child);
            parent = child;
        } else {
            break;
        }
    }

    return min;
}

int print_heap(const HEAP *heap) {
    int current_level = 1;
    ITEM* item;

    for (int i=0; i<heap->size; ++i) {
        item = get_da(heap->array, i);
        printf("(%d, %c) ", item->key, item->value);
        if (i == floor(pow(2, current_level)) - 2) {
            printf("\n");
            ++current_level;
        }
    }
    printf("\n");

    return 0;
}
