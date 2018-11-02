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
typedef struct leaf_s LEAF;
struct leaf_s {
    int data;
    /* RED or BLACK */
    char colour;
    LEAF* parent;
    LEAF* left;
    LEAF* right;
};

/* a tree is a pointer to the root leaf */
typedef struct tree_s {
    LEAF* root;
    int size;
} TREE;

/* construct a new coloured leaf containing integer */
LEAF* new_leaf(int n, char clr) {
    if (clr != RED && clr != BLACK) {
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
    new->root = NULL;
    new->size = 0;
    return new;
}

/* recursive function to delete a leaf and all its children */
int free_leaf_rec(LEAF **leaf) {
    if (*leaf == NULL) {
        printf("Passed a NULL leaf: TREE.FREE_LEAF_REC\n");
        return ERR_VAL;
    }
    /* free left and right subtrees */
    if ((*leaf)->left != NULL) {
        free_leaf_rec(&((*leaf)->left));
    }
    if ((*leaf)->right != NULL) {
        free_leaf_rec(&((*leaf)->right));
    }
    /* free current leaf */
    free(*leaf);
    *leaf = NULL;

    return 0;
}

/* delete tree and free memory */
int free_tree(TREE **tree) {
    if (*tree == NULL) {
        printf("Passed a NULL tree: TREE.FREE_TREE\n");
        return ERR_VAL;
    }
    /* free all leaves of tree */
    if ((*tree)->root != NULL) {
        free_leaf_rec(&((*tree)->root));
    }
    free(*tree);
    *tree = NULL;

    return 0;
}

/* print leaves of tree recursively */
int print_leaf_rec(const LEAF *leaf) {
    if (leaf == NULL) {
        printf("Passed a NULL leaf: TREE.PRINT_LEAF_REC\n");
        return ERR_VAL;
    }
    printf("(%d", leaf->data);
    if (leaf->left == NULL) {
        printf(" ()");
    } else {
        printf(" ");
        print_leaf_rec(leaf->left);
    }
    if (leaf->right == NULL) {
        printf(" ()");
    } else {
        printf(" ");
        print_leaf_rec(leaf->right);
    }
    printf(")");

    return 0;
}

/* print a tree */
int print_tree(const TREE *tree) {
    if (tree == NULL) {
        printf("Passed a NULL tree: TREE.PRINT_TREE\n");
        return ERR_VAL;
    }
    print_leaf_rec(tree->root);
    printf("\n");
    return 0;
}

/* helper function to sink the leaf to the right spot in BST */
int sink(LEAF* leaf, LEAF* curr_node) {
    if (leaf->data == curr_node->data) {
        printf("Encountered duplicate: TREE.SINK");
        return ERR_VAL;
    }
    if (leaf->data < curr_node->data) {
        if (curr_node->left == NULL) {
            curr_node->left = leaf;
            leaf->parent = curr_node;
        } else {
            return sink(leaf, curr_node->left);
        }
    } else {
        if (curr_node->right == NULL) {
            curr_node->right = leaf;
            leaf->parent = curr_node;
        } else {
            return sink(leaf, curr_node->right);
        }
    }

    return 0;
}


/* insert a new integer into tree */
int insert(TREE *tree, int n) {
    /* create a new leaf */
    LEAF *leaf = new_leaf(n, RED);
    if (tree->root == NULL) {
        tree->root = leaf;
    } else {
        sink(leaf, tree->root);
    }

    ++(tree->size);
    return 0;
}

int main() {
    TREE *tree = new_tree();

    insert(tree, 8);
    insert(tree, 3);
    insert(tree,10);
    insert(tree,6);
    insert(tree,9);

    print_tree(tree);

    return 0;
}
