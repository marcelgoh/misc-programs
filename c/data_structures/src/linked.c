/* Singly linked list (of int) implementation in C
 * Last updated by Marcel Goh on 16 October 2018
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "linked.h"

#define ERR_VAL INT_MIN

/* a single node in the list */
typedef struct node_s {
    int data;
    struct node_s *next;
} NODE;

/* pointers to the first and last in linked list */
typedef struct linked_list_s {
    NODE *head, *tail;
    int size;
} LL;

/* helper procedure to create a new node containing int*/
NODE *new_node(int n) {
    NODE *new = (NODE*) malloc(sizeof(NODE));
    new->data = n;
    new->next = NULL;
    return new;
}

/* helper procedure to construct a new empty linked list */
LL *new_ll() {
    LL *new = (LL*) malloc(sizeof(LL));
    new->head = new->tail = NULL;
    new->size = 0;
    return new;
}

/* delete a linked list and free memory */
int free_ll(LL **list) {
    if (*list == NULL) {
        printf("Passed a NULL list: LINKED.FREE_LL\n");
        return ERR_VAL;
    }
    NODE *curr = (*list)->head;
    NODE *temp;

    /* free every node in the list */
    while (curr != NULL) {
        temp = curr;
        curr = curr->next;
        free(temp);
    }
    /* free the linked list */
    free(*list);
    *list = NULL;


    return 0;
}

/* add node containing n to the front of the list */
int add_first(LL *list, int n) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.ADD_FIRST\n");
        return ERR_VAL;
    }
    NODE *new = new_node(n);

    if (list->head == NULL) {
        /* add to an empty list */
        list->head = new;
        list->tail = new;
    } else {
        new->next = list->head;
        list->head = new;
    }
    ++(list->size);

    return 0;
}

/* add node containing n to the end of the list */
int add_last(LL *list, int n) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.ADD_LAST\n");
        return ERR_VAL;
    }
    NODE *new = new_node(n);

    if (list->head == NULL) {
        list->head = new;
        list->tail = new;
    } else {
        list->tail->next = new;
        list->tail = new;
    }
    ++(list->size);

    return 0;
}

/* return (but don't remove) the first int in the list */
int get_first(const LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.GET_FIRST\n");
        return ERR_VAL;
    }
    if (list->head == NULL) {
        printf("List is empty: LINKED.GET_FIRST\n");
        return ERR_VAL;
    } else {
        return list->head->data;
    }
}

/* return the last int in the list */
int get_last(const LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.GET_LAST\n");
        return ERR_VAL;
    }
    if (list->tail == NULL) {
        printf("List is empty: LINKED.GET_LAST\n");
        return ERR_VAL;
    } else {
        return list->tail->data;
    }
}

/* return the value at index */
int get_val_at(const LL *list, int index) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.GET_VAL_AT\n");
        return ERR_VAL;
    }
    if (index == 0) {
        return get_first(list);
    } else if (index < 0 || index >= get_size_ll(list)) {
        printf("Illegal index: LINKED.GET_VAL_AT\n");
        return ERR_VAL;
    } else if (list->head == NULL) {
        printf("List is empty: LINKED.GET_VAL_AT\n");
        return ERR_VAL;
    } else {
        NODE *curr = list->head;
        for (int i=0; i<index; ++i) {
            curr = curr->next;
        }
        return curr->data;
    }
}

/* get the current size of list */
int get_size_ll(const LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.GET_SIZE_LL\n");
        return ERR_VAL;
    }
    return list->size;
}

/* remove (and return) first int in list */
int remove_first(LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.REMOVE_FIRST\n");
        return ERR_VAL;
    }
    if (list->head == NULL) {
        printf("List is empty: LINKED.REMOVE_FIRST\n");
        return ERR_VAL;
    } else {
        int ret_val = list->head->data;
        NODE *new_head = list->head->next;

        /* delete the head node and point to new head */
        free(list->head);
        list->head = new_head;
        --(list->size);

        if (list->size == 0) {
            list->head = list-> tail = NULL;
        }
        return ret_val;
    }
}

/* remove (and return) int at index (runs in linear time) */
int remove_val_at(LL *list, int index) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.REMOVE_VAL_AT\n");
        return ERR_VAL;
    }
    if (index == 0) {
        return remove_first(list);
    } else if (index < 0 || index >= get_size_ll(list)) {
        printf("Illegal index: LINKED.REMOVE_VAL_AT\n");
        return ERR_VAL;
    } else if (list->head == NULL) {
        printf("List is empty: LINKED.REMOVE_VAL_AT\n");
        return ERR_VAL;
    } else {
        /* find the node just before the one we want to remove */
        NODE *before = list->head;
        for (int i=0; i<index-1; ++i) {
            before = before->next;
        }
        /* delete target node */
        NODE *target = before->next;
        int ret_val = target->data;
        before->next = target->next;
        free(target);
        --(list->size);

        if (list->size == 0) {
            list->head = list-> tail = NULL;
        }
        return ret_val;
    }
}

/* remove and return last int in list (linear) */
int remove_last(LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.REMOVE_LAST\n");
        return ERR_VAL;
    }
    return remove_val_at(list, get_size_ll(list) - 1);
}

/* print all elements of list to screen */
int print_ll(const LL *list) {
    if (list == NULL) {
        printf("Passed a NULL list: LINKED.PRINT_LL\n");
        return ERR_VAL;
    }
    NODE *curr = list->head;
    while (curr != NULL) {
        printf("%d ", curr->data);
        curr = curr->next;
    }
    printf("\n");

    return 0;
}

/* testing
int main() {
    LL *list = new_ll();
    add_first(list, 1);
    add_first(list, 3);
    add_last(list, 4);
    add_last(list, 10);
    add_last(list, 4);
    printf("%d\n", get_val_at(list, 2));
    print_ll(list);
    remove_val_at(list, 4);
    free_ll(&list);
    printf("%d\n", (list == NULL) ? 1 : 0);
    print_ll(list);

    return 0;
}
*/
