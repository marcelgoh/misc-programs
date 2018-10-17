/* header file for linked.c: linked list implementation in C */

/* single node in list */
typedef struct node_s NODE;

/* data structure for list */
typedef struct linked_list_s LL;


/* create a new node with integer data */
NODE *new_node(int n);

/* create an empty list */
LL *new_ll();

/* delete a list and free memory */
int free_ll(LL *list);

/* add int to front of list */
int add_first(LL *list, int n);

/* add int to end of list */
int add_last(LL *list, int n);

/* get first value in list */
int get_first(const LL *list);

/* get last value in list */
int get_last(const LL *list);

/* get the value at index */
int get_val_at(const LL *list, int index);

/* get the number of elements in the list */
int get_size_ll(const LL *list);

/* remove and return the first int in the list */
int remove_first(LL *list);

/* remove and return value at index */
int remove_val_at(LL *list, int index);

/* remove and return last int in list */
int remove_last(LL *list);

/* print contents of list to screen in order from first to last */
int print_ll(const LL *list);
