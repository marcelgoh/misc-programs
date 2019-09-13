/* Hashtable data structure
 * Last updated 13 September 2019 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashtbl.h"

#define ERR_VAL INT_MIN

/* hash function: djb2 */
unsigned long hash(char *str) {
    unsigned long hash = 5381;
    int c;

    while ((c = *str++) != '\0') {
        hash = ((hash << 5) + hash) + c;
    }

    return hash;
}

/* bucket data structure */
struct bucket {
    char *key;
    char value;
    struct bucket *next;
};

/* creates a new bucket */
struct bucket *new_bucket(char *newkey, char newvalue) {
    struct bucket *b = malloc(sizeof(struct bucket));
    b->key = newkey;
    b->value = newvalue;
    b->next = NULL;

    return b;
}

/* frees a bucket pointer */
int free_bucket(struct bucket **b) {
    if (*b == NULL) {
        printf("Cannot delete NULL bucket: HASHTBL.DELETE_BUCKET\n");
        return ERR_VAL;
    }

    if ((*b)->next != NULL) {
        free_bucket(&((*b)->next));
    }

    free(*b);
    *b = NULL;

    return 0;
}

/* creates a new hashtable with fixed number of buckets */
HASHTBL *new_hashtbl(int capacity) {
    HASHTBL *htbl = malloc(sizeof(HASHTBL));
    htbl->capacity = capacity;
    htbl->load = 0;
    htbl->buckets = malloc(capacity*sizeof(struct bucket));
    for (int i=0; i<capacity; ++i) {
        htbl->buckets[i] = NULL;
    }

    return htbl;
}

/* frees a hashtable pointer */
int free_hashtbl(HASHTBL **h) {
    if (*h == NULL) {
        printf("Cannot delete NULL hashtable: HASHTBL.DELETE_BUCKET\n");
        return ERR_VAL;
    }

    for (int i=0; i<(*h)->capacity; ++i) {
        if ((*h)->buckets[i] != NULL) {
            free((*h)->buckets[i]);
        }
    }

    free(*h);
    *h = NULL;

    return 0;
}

/* replaces value of k in hashtable h with v if k is already in the table;
 * adds (k,v) if not
 */
int tbl_replace(HASHTBL *h, char *k, char v) {
    if (h == NULL) {
        printf("Cannot perform find on null hashtable: HASHTBL.TBL_REPLACE\n");
        return ERR_VAL;
    }
    unsigned long x = hash(k);
    int n = x % h->capacity;

    struct bucket *b = h->buckets[n];

    if (b == NULL) {
        h->buckets[n] = new_bucket(k, v);
        if (h->load > 0.75 * h->capacity) {
            printf("Warning: hashtable becoming overloaded.\n");
        }
        ++(h->load);
    } else {
        while (b != NULL) {
            if (strcmp(b->key, k) == 0) {
                b->value = v;
                break;
            }
            b = b->next;
        }
    }

    return 0;
}

/* if k is in the table h, then returns the value v corresponding to it;
 * otherwise, returns char 0
 */
char tbl_find(HASHTBL *h, char *k) {
    if (h == NULL) {
        printf("Cannot perform find on null hashtable: HASHTBL.TBL_FIND\n");
        return '\0';
    }
    unsigned long x = hash(k);
    int n = x % h->capacity;

    struct bucket *b = h->buckets[n];

    while (b != NULL) {
        if (strcmp(b->key, k) == 0) {
            return b->value;
        }
        b = b->next;
    }

    return '\0';
}
