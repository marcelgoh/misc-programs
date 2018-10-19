/* Header file for stack.c: linked-list implementation of a stack
 * Last updated by Marcel Goh on 17 October 2018
 */

/* stack representation */
typedef struct stack_s STACK;


/* construct a new stack */
STACK* new_stack();

/* delete stack and free memory */
int free_stack(STACK **stack);

/* push integer onto stack */
int push(STACK *stack, int n);

/* pop integer from stack */
int pop(STACK *stack);

/* get (but don't pop) first integer on stack */
int peek(const STACK *stack);

/* returns 1 if stack is empty and 0 otherwise */
int is_empty_stack(const STACK *stack);

/* print contents of stack in FIFO order */
int print_stack(const STACK *stack);
