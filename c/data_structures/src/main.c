/* Main procedure that uses data structures
 * Last updated on 17 October 2018
 */

#include <stdio.h>

#include "linked.h"
#include "queue.h"
#include "stack.h"

int main() {
    /* STACK TESTS */
    STACK *stack = new_stack();
    push(stack, 4);
    push(stack, 1);
    push(stack, 3);
    print_stack(stack);
    printf("Popped a %d\n", pop(stack));
    push(stack, 2);
    print_stack(stack);
    printf("Popped a %d\n", pop(stack));
    printf("Popped a %d\n", pop(stack));
    printf("Peeked at a %d\n", peek(stack));
    print_stack(stack);
    free_stack(&stack);
    printf("%d\n", (stack == NULL) ? 1 : 0);
    printf("Peeked at a %d\n", peek(stack));

    /* QUEUE TESTS
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
    */

    return 0;
}
