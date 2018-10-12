/* Singly linked list implementation in C
 * Written by Marcel Goh on 12 October 2018
 */

#include <stdlib.h>
#include "linked.h"

/* a single node in the list */
typedef struct node_s {
    int data;
    struct q_node *next;
} NODE;

/* pointers to the first and last in linked list */
typedef struct linked_list_s {
    NODE *head, *tail;
} LL;

/* helper procedure to create a new node containing int*/
NODE* new_node(int n) {
    NODE *new = (NODE*) malloc(sizeof(NODE));
    new->data = n;
    new->next = NULL;
    return new;
}

/* helper procedure to construct a new linked list */
LL* new_ll() {
    LL *new = (LL*) malloc(sizeof(LL));
    new->head = new->tail = NULL;
    return new;
}



/* delete a linked list and free memory */
int free_ll(LL* list) {
    NODE* first = list->head;

    /* free every node in the list */
    while (first != NULL) {
        temp = head;
        head = head->next;
        free(temp);
    }
    /* free the linked list */
    free(list);

    return 0;
}

/* add node containing n to the front of the list */
int add_first(LL* list, int n) {
    NODE *new = new_node();

    new->data = n;
    new->next = list->head;

    list->head = new;

    return 0;
}
