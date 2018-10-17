/* Linked-list implementation of a queue
 * Last updated by Marcel Goh on 17 October 2018
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "linked.h"
#include "queue.h"

#define ERR_VAL INT_MIN

/* a queue is represented as a linked list */
typedef struct queue_s {
    LL *list;
} QUEUE;

/* construct a new queue */
QUEUE* new_queue() {
    QUEUE *new = (QUEUE*) malloc(sizeof(QUEUE));
    new->list = new_ll();
    return new;
}

/* delete queue and free memory */
int free_queue(QUEUE **q) {
    if (*q == NULL) {
        printf("Passed a NULL queue: QUEUE.FREE_QUEUE\n");
        return ERR_VAL;
    }
    free_ll(&((*q)->list));
    free(*q);
    *q = NULL;

    return 0;
}

/* add int n to end of queue */
int enqueue(QUEUE *q, int n) {
    if (q == NULL) {
        printf("Passed a NULL queue: QUEUE.ENQUEUE\n");
        return ERR_VAL;
    }
    return add_last(q->list, n);
}

/* remove and return int at front of queue */
int dequeue(QUEUE *q) {
    if (q == NULL) {
        printf("Passed a NULL queue: QUEUE.DEQUEUE\n");
        return ERR_VAL;
    }
    int ret_val = remove_first(q->list);
    if (ret_val == ERR_VAL) {
        printf("Error: QUEUE.DEQUEUE\n");
    }
    return ret_val;
}

/* get (but don't remove) value at front of queue */
int fetch(const QUEUE *q) {
    if (q == NULL) {
        printf("Passed a NULL queue: QUEUE.FETCH\n");
        return ERR_VAL;
    }
    int ret_val = get_first(q->list);
    if (ret_val == ERR_VAL) {
        printf("Error: QUEUE.FETCH\n");
    }
    return ret_val;
}

/* print contents of queue in LIFO order */
int print_queue(const QUEUE *q) {
    if (q == NULL) {
        printf("Passed a NULL queue: QUEUE.PRINT_QUEUE\n");
        return ERR_VAL;
    }
    return print_ll(q->list);
}

/* testing
int main() {
    QUEUE *q = new_queue();
    enqueue(q, 5);
    enqueue(q, 14);
    print_queue(q);
    printf("Dequeued a %d\n", dequeue(q));
    enqueue(q, 17);
    print_queue(q);
    printf("Dequeued a %d\n", dequeue(q));
    printf("Fetched a %d\n", fetch(q));
    free_queue(&q);
    printf("%d\n", (q == NULL) ? 1: 0);
    printf("Fetched a %d\n", fetch(q));

    return 0;
}
*/
