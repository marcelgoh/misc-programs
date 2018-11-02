/* Red-black tree data-structure
 * Last updated 17 October 2018 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL INT_MIN
#define RED 'r'
#define BLACK 'b'

/* a single leaf on the tree (ALL NODES ARE CALLED LEAVES) */
typedef struct leaf_s {
    int data;
    /* RED or BLACK */
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

/* recursive function to delete a leaf and all its children */
int free_leaf_rec(LEAF **leaf) {
    if (*leaf == NULL) {
        printf("Passed a NULL leaf: FREE_LEAF_REC\n");
        return ERR_VAL;
    }
    /* free left and right subtrees */
    if ((*leaf)->left != NULL) {
        free_leaf_rec((*leaf)->left);
    }
    if ((*leaf)->right != NULL) {
        free_leaf_rec((*leaf)->right);
    }
    /* free current leaf */
    free(*leaf);
    *leaf = NULL;

    return 0;
}

/* delete tree and free memory */
int free_tree(TREE **tree) {
    if (*tree == NULL) {
        printf("Passed a NULL tree: FREE_TREE\n");
        return ERR_VAL;
    }
    /* free all leaves of tree */
    if ((*tree)->root != NULL) {
        free_leaf_rec((*tree)->root);
    }
    free(*tree);
    *tree = NULL;

    return 0;
}

/* insert a new integer into tree */
int insert(TREE *tree, int n) {
    return 0;
}
