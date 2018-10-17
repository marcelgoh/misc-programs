/* Linked-list implementation of a stack
 * Last updated 17 October 2018 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include "linked.h"

#define ERR_VAL INT_MIN

/* underlying representation is a linked list */
typedef struct stack_s {
    LL *list;
} STACK;

/* construct a new stack */
STACK* new_stack() {
    STACK *new = (STACK*) malloc(sizeof(STACK));
    new->list = new_ll();
    return new;
}

/* delete stack and free memory */
int free_stack(STACK **stack) {
    if (*stack == NULL) {
        printf("Passed a NULL stack: STACK.FREE_STACK\n");
        return ERR_VAL;
    }
    free_ll(&((*stack)->list));
    free(*stack);
    *stack = NULL;

    return 0;
}

/* push an integer onto the stack */
int push(STACK *stack, int n) {
    if (stack == NULL) {
        printf("Passed a NULL stack: STACK.PUSH\n");
        return ERR_VAL;
    }
    return add_first(stack->list, n);
}

/* pop an integer from the stack */
int pop(STACK *stack) {
    if (stack == NULL) {
        printf("Passed a NULL stack: STACK.POP\n");
        return ERR_VAL;
    }
    int ret_val = remove_first(stack->list);
    if (ret_val == ERR_VAL) {
        printf("Error: STACK.POP\n");
    }
    return ret_val;
}

/* get (but don't pop) the integer at the top of stack */
int peek(const STACK *stack) {
    if (stack == NULL) {
        printf("Passed a NULL stack: STACK.PEEK\n");
        return ERR_VAL;
    }
    int ret_val = get_first(stack->list);
    if (ret_val == ERR_VAL) {
        printf("Error: STACK.PEEK\n");
    }
    return ret_val;
}

/* print contents of stack in FIFO order */
int print_stack(const STACK *stack) {
    if (stack == NULL) {
        printf("Passed a NULL stack: STACK.PRINT_STACK\n");
        return ERR_VAL;
    }
    return print_ll(stack->list);
}
