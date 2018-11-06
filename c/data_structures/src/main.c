/* Main procedure that uses data structures
 * Last updated on 17 October 2018
 */

#include <stdio.h>

#include "linked.h"
#include "queue.h"
#include "stack.h"
#include "tree.h"

int main() {
    /* RED-BLACK TREE TESTS */
    TREE *tree = new_tree();

    insert(tree,8,'h');
    insert(tree,3,'c');
    insert(tree,12,'l');
    insert(tree,6,'f');
    insert(tree,9,'i');
    insert(tree,5,'e');
    insert(tree,13,'m');
    insert(tree,1,'a');
    insert(tree,7,'g');
    insert(tree,2,'b');
    insert(tree,4,'d');
    insert(tree,10,'j');
    insert(tree,11,'k');

    print_tree(tree);
    printf("%d\n", tree->size);

    delete(tree, 11);
    print_tree(tree);
    delete(tree, 9);
    print_tree(tree);
    delete(tree, 13);
    print_tree(tree);
    delete(tree, 4);
    print_tree(tree);
    delete(tree, 1);
    print_tree(tree);
    delete(tree, 2);
    print_tree(tree);
    delete(tree, 3);
    print_tree(tree);
    delete(tree, 6);
    print_tree(tree);
    delete(tree, 7);
    print_tree(tree);
    delete(tree, 8);
    print_tree(tree);
    delete(tree, 5);
    print_tree(tree);
    delete(tree, 10);
    print_tree(tree);
    delete(tree, 12);
    print_tree(tree);

    printf("%d\n", tree->size);

    /* LINKED LIST TESTS
    LL *list = new_ll();
    print_ll(list);
    reverse_ll(list);
    print_ll(list);
    add_first(list, 1);
    add_first(list, 3);
    print_ll(list);
    reverse_ll(list);
    print_ll(list);
    add_last(list, 4);
    add_last(list, 10);
    add_last(list, 4);
    printf("%d\n", get_val_at(list, 2));
    print_ll(list);
    reverse_ll(list);
    print_ll(list);
    remove_val_at(list, 4);
    free_ll(&list);
    */

    /* STACK TESTS
    STACK *stack = new_stack();
    printf("Is stack empty? %d\n", is_empty_stack(stack));
    push(stack, 4);
    printf("Is stack empty? %d\n", is_empty_stack(stack));
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
    printf("Is stack empty? %d\n", is_empty_stack(stack));
    printf("%d\n", (stack == NULL) ? 1 : 0);
    printf("Peeked at a %d\n", peek(stack));
    */

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
