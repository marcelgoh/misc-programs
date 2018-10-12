/* C linked-list implementation of a queue
 * Written by Marcel Goh on 12 October 2018
 */

#include <stdio.h>
#include <stdlib.h>

/* a single node in the queue */
typedef struct q_node {
    int data;
    struct q_node *next;
} NODE;

/* pointers to the first and last in queue */
typedef struct queue {
    NODE *head, *tail;
} QUEUE;

/* helper procedure to create a new node containing int*/
NODE* new_node(int n) {
    NODE *new = (NODE*) malloc(sizeof(NODE));
    new->data = n;
    new->next = NULL;
    return new;
}

/* helper procedure to construct a new queue */
QUEUE* new_queue() {
    QUEUE *new = (QUEUE*) malloc(sizeof(QUEUE));
    new->head = new->tail = NULL;
    return new;
}

/* ENQUEUE: add int n to end of queue */
int enqueue(QUEUE *q, int n) {
    /* create a new node containing n */
    NODE* node = new_node(n);

    /* case: if queue is empty */
    if (q->tail == NULL) {
        q->head = q->tail = node;
        return 0;
    }

    /* else: add new node to end of queue and point previous
     *       tail to new tail
     */
    q->tail->next = node;
    q->tail = node;
    return 0;
}

/* DEQUEUE: remove and return node at head */
NODE* dequeue(QUEUE *q) {
    /* case: if queue is empty */
    if (q->head == NULL) {
        return NULL;
    }

    /* get the head of queue and remove it */
    NODE* ret_node = q->head;
    q->head = q->head->next;

    /* case: if head becomes NULL, list now empty */
    if (q->head == NULL) {
        q->tail = NULL;
    }

    return ret_node;
}

int print_dequeue(QUEUE *q) {
    NODE *n = dequeue(q);
    if (n == NULL) {
        printf("Empty list: PRINT_DEQUEUE\n");
        return -1;
    } else {
        printf("Dequeued %d\n", n->data);
        return 0;
    }
}

int main() {
    QUEUE *q = new_queue();
    enqueue(q, 5);
    enqueue(q, 14);
    dequeue(q);
    enqueue(q, 17);
    print_dequeue(q);

    return 0;
}
