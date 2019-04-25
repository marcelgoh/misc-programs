/* Resizing array data-structure interface
 * Last updated 25 April 2019 by Marcel Goh
 */

#define ERR_VAL INT_MIN

/* an item is an (int, char) pair */
typedef struct item_s {
    int key;
    char value;
} ITEM;

/* returns a new item */
ITEM* new_item(int k, char v);

/* resizing array of items */
typedef struct dynarray_s DYNARRAY;

/* create a new dynamic array */
DYNARRAY* new_da(int start_cap);

/* delete an array */
int free_da(DYNARRAY **da);

/* returns number of elements in array */
int size_da(const DYNARRAY *da);

/* get item at index */
ITEM *get_da(const DYNARRAY *da, int idx);

/* set item at index */
int set_da(DYNARRAY *da, int idx, int key, char value);

/* insert item at index, shifting everything after it down */
int insert_da(DYNARRAY *da, int idx, int key, char value);

/* removes (but doesn't return) element at index */
int remove_da(DYNARRAY *da, int idx);

/* add a single element to the end of an array */
int add_da(DYNARRAY *da, int key, char value);

/* print keys and values */
int print_da(const DYNARRAY *da);
