/* Hashtable data structure
 * Last updated 13 September 2019 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL INT_MIN

/* fixed-size hashtable structure */
typedef struct hashtbl {
    struct bucket **buckets;
    int capacity;
    int load;
} HASHTBL;

/* creates a new hashtable with fixed capacity */
HASHTBL *new_hashtbl(int capacity);

/* frees a hashtable pointer */
int free_hashtbl(HASHTBL **h);

/* replaces value of k in hashtable h with v if k is already in the table;
 * adds (k,v) if not
 */
int tbl_replace(HASHTBL *h, char *k, char v);

/* if k is in the table h, then returns the value v corresponding to it;
 * otherwise, returns char 0
 */
char tbl_find(HASHTBL *h, char *k);
