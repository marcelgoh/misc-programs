/* Red-black tree data-structure
 * Last updated 17 October 2018 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL INT_MIN
#define RED 'r'
#define BLACK 'b'

/* a single leaf on the tree */
typedef struct leaf_s {
    int data;
    /* 'r'ed or 'b'lack */
    char colour;
    LEAF* parent;
    LEAF* left;
    LEAF* right;
} LEAF;

/* a tree is a pointer to the root leaf */
typedef struct tree_s {
    LEAF* root;
    int size;
} TREE;

/* construct a new coloured leaf containing integer */
LEAF* new_leaf(int n, char clr) {
    if (clr != RED || clr != BLACK) {
        printf("Colour must be red or black: TREE.NEW_LEAF\n");
        return NULL;
    }
    LEAF* new = (LEAF*) malloc(sizeof(LEAF));
    new->data = n;
    new->colour = clr;
    new->parent = new->left = new->right = NULL;
    return new;
}

/* construct a new (empty) tree */
TREE* new_tree() {
    TREE* new = (TREE*) malloc(sizeof(TREE));
    tree->root = NULL;
    tree->size = 0;
    return tree;
}
