/* Header file for queue.c: Linked-list queue */

/* a queue represented as linked list */
typedef struct queue QUEUE;


/* construct a new queue */
QUEUE* new_queue();

/* delete queue and free memory */
int free_queue(QUEUE **q);

/* add int to queue */
int enqueue(QUEUE *q, int n);

/* remove and return int at front of queue */
int dequeue(QUEUE *q);

/* get value at the front without modifying queue */
int fetch(const QUEUE *q);

/* print queue in LIFO order */
int print_queue(const QUEUE *q);
