/* Main procedure that uses data structures
 * Last updated on 26 April 2019
 */

#include <stdio.h>

#include "linked.h"
#include "queue.h"
#include "stack.h"
#include "tree.h"
#include "dynarray.h"
#include "heap.h"
#include "hashtbl.h"

int main() {
    /* HASHTBL TESTS */
    HASHTBL *h = new_hashtbl(100);
    tbl_replace(h, "hello", 'c');
    tbl_replace(h, "knuth", 'g');
    printf("%c%c\n", tbl_find(h, "hello"), tbl_find(h, "knuth"));
    tbl_replace(h, "hello", 'm');
    printf("%c%c\n", tbl_find(h, "hello"), tbl_find(h, "knuth"));
    free_hashtbl(&h);
    tbl_find(h, "hello");

    /* HEAP TESTS
    HEAP *h = new_heap();
    add_heap(h, 4, 'd');
    add_heap(h, 6, 'f');
    add_heap(h, 2, 'b');
    print_heap(h);
    add_heap(h, 8, 'h');
    add_heap(h, 9, 'i');
    add_heap(h, 3, 'c');
    add_heap(h, 5, 'e');
    add_heap(h, 1, 'a');
    print_heap(h);
    printf("\n");
    remove_min(h);
    add_heap(h, 7, 'g');
    print_heap(h);
    free_heap(&h);
    print_heap(h);
    */

    /* DYNARRAY TESTS
    DYNARRAY *arr = new_da(5);
    add_da(arr, 7, 'l');
    add_da(arr, 5, 'i');
    add_da(arr, 4, 'b');
    add_da(arr, 2, 'i');
    add_da(arr, 3, 'p');
    add_da(arr, 2, 'm');
    print_da(arr);
    printf("%d\n", size_da(arr));
    ITEM* item = get_da(arr, 3);
    printf("%d %c\n", item->key, item->value);
    set_da(arr, 2, 10, 'a');
    print_da(arr);
    insert_da(arr, 5, 6, 'c');
    print_da(arr);
    remove_da(arr, 2);
    print_da(arr);
    swap_da(arr, 4, 2);
    print_da(arr);
    free_da(&arr);
    print_da(arr);
    */

    /* RED-BLACK TREE TESTS 
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

    printf("%c\n", delete(tree, 13));
    printf("%c\n", delete(tree, 21));
    */

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
    print_ll(list);
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
