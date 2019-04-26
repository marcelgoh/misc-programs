/* Min-heap interface
 * Last updated 26 April 2019 by Marcel Goh
 */

/* heap data structure (uses arrays) */
typedef struct heap_s HEAP;

/* create a new min-heap */
HEAP* new_heap();

/* delete a min-heap */
int free_heap(HEAP **heap);

/* add a key-value pair to heap */
int add_heap(HEAP *heap, int key, char value);

/* returns min element of heap */
ITEM* peek_min(const HEAP *heap);

/* removes and returns min element of heap */
ITEM* remove_min(HEAP *heap);

/* print a heap to console */
int print_heap(const HEAP *heap);
