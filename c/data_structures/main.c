#include <stdio.h>
#include "linked.h"

int main() {
    LL *l = new_ll();
    enqueue(q, 5);
    enqueue(q, 14);
    dequeue(q);
    enqueue(q, 17);
    print_dequeue(q);

    return 0;
}
