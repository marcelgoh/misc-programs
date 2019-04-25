/* Resizing array data-structure
 * Last updated 25 April 2019 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "dynarray.h"

#define ERR_VAL INT_MIN

/* an item is an (int, char) pair */
typedef struct item_s ITEM;

/* returns a new item */
ITEM* new_item(int k, char v) {
    ITEM* new = (ITEM*) malloc(sizeof(ITEM));
    new->key = k;
    new->value = v;
    return new;
}

/* resizing array of items */
typedef struct dynarray_s {
    ITEM* array;
    int size;        /* number of filled slots */
    int capacity;    /* number of slots total */
} DYNARRAY;

/* create a new dynamic array */
DYNARRAY* new_da(int start_cap) {
    DYNARRAY *new = (DYNARRAY*) malloc(sizeof(DYNARRAY));
    new->array = (ITEM*) malloc(start_cap * sizeof(ITEM));
    new->size = 0;
    new->capacity = start_cap;
    return new;
}

/* delete an array */
int free_da(DYNARRAY **da) {
    if (*da == NULL) {
        printf("Passed a null array: DYNARRAY.FREE_DYNARRAY()\n");
        return ERR_VAL;
    }
    free((*da)->array);
    free(*da);
    *da = NULL;

    return 0;
}

/* doubles the internal capacity of a dynamic array */
int increase_cap(DYNARRAY **da) {
    if (*da == NULL) {
        printf("Passed a null array: DYNARRAY.INCREASE_CAP()\n");
        return ERR_VAL;
    }
    (*da)->capacity = 2 * (*da)->capacity;
    ITEM *new = (ITEM*) malloc((*da)->capacity * sizeof(ITEM));
    ITEM *old = (*da)->array;
    for (int i=0; i<(*da)->size; ++i) {
        *(new + i) = *(old + i);
    }
    (*da)->array = new;

    return 0;
}

/* halves the internal capacity of a dynamic array */
int decrease_cap(DYNARRAY **da) {
    if (*da == NULL) {
        printf("Passed a null array: DYNARRAY.DECREASE_CAP()\n");
        return ERR_VAL;
    }
    if ((*da)->size < (*da)->capacity / 2) {
        printf("Too many elements in array: DYNARRAY.DECREASE_CAP()\n");
        return ERR_VAL;
    }
    (*da)->capacity = (*da)->capacity / 2;
    ITEM *new = (ITEM*) malloc((*da)->capacity * sizeof(ITEM));
    ITEM *old = (*da)->array;
    for (int i=0; i<(*da)->size; ++i) {
        *(new + i) = *(old + i);
    }
    (*da)->array = new;

    return 0;
}

/* returns number of elements in array */
int size_da(const DYNARRAY *da) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.SIZE_DA()\n");
        return ERR_VAL;
    }
    return da->size;
}

/* get item at index */
ITEM *get_da(const DYNARRAY *da, int idx) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.GET_DA()\n");
        return NULL;
    }
    if (idx >= da->size) {
        printf("Out of bounds of array: DYNARRAY.GET_DA()\n");
        return NULL;
    }
    return da->array+idx;
}

/* set item at index */
int set_da(DYNARRAY *da, int idx, int key, char value) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.SET_DA()\n");
        return ERR_VAL;
    }
    if (idx >= da->size) {
        printf("Out of bounds of array: DYNARRAY.SET_DA()\n");
        return ERR_VAL;
    }
    ITEM* new = new_item(key, value);
    *(da->array+idx) = *new;

    return 0;
}

int insert_da(DYNARRAY *da, int idx, int key, char value) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.INSERT_DA()\n");
        return ERR_VAL;
    }
    /* may need to resize underlying array */
    if (da->size == da->capacity) {
        increase_cap(&da);
    }
    if (idx > da->size) {
        printf("Out of bounds of array: DYNARRAY.INSERT_DA()\n");
    }
    ITEM* new = new_item(key, value);
    /* loop backwards from end of array until index, shifting forward */
    for (int i=da->size; i>idx; --i) {
        *(da->array + i) = *(da->array + i - 1);
    }
    *(da->array + idx) = *new;
    ++(da->size);


    return 0;
}

/* removes (but doesn't return) element at index */
int remove_da(DYNARRAY *da, int idx) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.REMOVE_DA()\n");
        return ERR_VAL;
    }
    if (idx >= da->size) {
        printf("Out of bounds of array: DYNARRAY.REMOVE_DA()\n");
    }
    /* starting at index, shift everything one down from the right */
    for (int i = idx; i<da->size-1; ++i) {
        *(da->array + i) = *(da->array + i + 1);
    }
    --(da->size);
    /* make underlying array smaller if below 1/3 capacity */
    if (da->size < da->capacity / 3) {
        decrease_cap(&da);
    }

    return 0;
}

/* add a single element to the end of an array */
int add_da(DYNARRAY *da, int key, char value) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.ADD_DA()\n");
        return ERR_VAL;
    }
    return insert_da(da, da->size, key, value);
}

/* print keys and values */
int print_da(const DYNARRAY *da) {
    if (da == NULL) {
        printf("Passed a null array: DYNARRAY.PRINT_DA()\n");
        return ERR_VAL;
    }
    printf("K:");
    for (int i=0; i<da->size; ++i) {
        printf(" %d", (da->array + i)->key);
    }
    printf("\n");
    printf("v:");
    for (int i=0; i<da->size; ++i) {
        printf(" %c", (da->array + i)->value);
    }
    printf("\n");

    return 0;
}
