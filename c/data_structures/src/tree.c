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
    int key;
    /* for simplicity's sake, all values are just chars, but
     * the data structure could be reimplemented with other data
     */
    char value;
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
LEAF* new_leaf(int k, char v, char clr) {
    if (clr != RED && clr != BLACK) {
        printf("Colour must be red or black: TREE.NEW_LEAF\n");
        return NULL;
    }
    LEAF* new = (LEAF*) malloc(sizeof(LEAF));
    new->key = k;
    new->value = v;
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
    printf("(%c", leaf->value);
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

/* return sibling of a leaf, NULL if none found */
LEAF* sibling(const LEAF *leaf) {
    LEAF *p = leaf->parent;
    if (p == NULL) {
        return NULL;
    }
    if (leaf == p->left) {
        return p->right;
    } else {
        return p->left;
    }
}

/* return grandparent of leaf, NULL if none found */
LEAF* grandparent(const LEAF *leaf) {
    LEAF *p = leaf->parent;
    if (p == NULL) {
        return NULL;
    } else {
        return p->parent;
    }
}

/* return uncle of leaf, NULL if none found */
LEAF* uncle(const LEAF *leaf) {
    LEAF *p = leaf->parent;
    if (p == NULL) {
        return NULL;
    } else {
        return sibling(p);
    }
}

/* rotate left at leaf */
int rotate_left(LEAF *leaf) {
    LEAF *new = leaf->right;
    /* if there is no right subtree, we cannot rotate left */
    if (new == NULL) {
        printf("Null right subtree: TREE.ROTATE_LEFT\n");
        return ERR_VAL;
    }
    LEAF *p = leaf->parent;
    /* perform rotation */
    leaf->right = new->left;
    new->left = leaf;
    leaf->parent = new;
    /* update leaf's right's parent pointer */
    if (leaf->right != NULL) {
        leaf->right->parent = leaf;
    }
    if (p != NULL) {
        /* replace leaf with new node in the parent leaf */
        if (leaf == p->left) {
            p->left = new;
        } else if (leaf == p->right) {
            p->right = new;
        }
    }
    new->parent = p;

    return 0;
}

/* rotate right at leaf */
int rotate_right(LEAF *leaf) {
    LEAF * new = leaf->left;
    /* if there is no left subtree, we cannot rotate right */
    if (new == NULL) {
        printf("Null left subtree: TREE.ROTATE_RIGHT\n");
        return ERR_VAL;
    }
    LEAF *p = leaf->parent;
    /* perform rotation */
    leaf->left = new->right;
    new->right = leaf;
    leaf->parent = new;
    /* update leaf's left's parent pointer */
    if (leaf->left != NULL) {
        leaf->left->parent = leaf;
    }
    if (p != NULL) {
        /* replace leaf with new node in the parent leaf */
        if (leaf == p->left) {
            p->left = new;
        } else if (leaf == p->right) {
            p->right = new;
        }
    }
    new->parent = p;

    return 0;
}


/* helper function to sink the leaf to the right spot in BST */
int sink(LEAF* leaf, LEAF* curr_node) {
    /* if the key already exists in the tree, replace its value with new value */
    if (leaf->key == curr_node->key) {
        curr_node->value = leaf->value;
    } else if (leaf->key < curr_node->key) {
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

/* insert a new key-value pair into tree */
int insert(TREE *tree, int k, char v) {
    /* create a new leaf */
    LEAF *leaf = new_leaf(k, v, RED);
    if (tree->root == NULL) {
        tree->root = leaf;
        leaf->colour = BLACK;
    } else {
        sink(leaf, tree->root);
    }

    ++(tree->size);
    return 0;
}

/* recursive helper function to get value associated with key */
LEAF* search(int k, LEAF *curr_node) {
    if (curr_node == NULL) {
        return NULL;
    }
    int this_key = curr_node->key;
    if (k == this_key) {
        return curr_node;
    } else if (k < this_key) {
        return search(k, curr_node->left);
    } else {
        return search(k, curr_node->right);
    }
}

/* finds and returns the value associated with a certain key
 * returns '\0' if key not found
 */
char find(const TREE *tree, int k) {
    if (tree == NULL) {
        return '\0';
    }
    LEAF *node = search(k, tree->root);

    if (node == NULL) {
        return '\0';
    } else {
        return node->value;
    }
}

int main() {
    TREE *tree = new_tree();

    insert(tree,8,'h');
    insert(tree,3,'c');
    insert(tree,10,'j');
    insert(tree,6,'f');
    insert(tree,9,'i');
    insert(tree,5,'e');
    insert(tree,1,'a');
    insert(tree,7,'g');

    print_tree(tree);

    LEAF* three = search(3, tree->root);
    rotate_right(three);
    print_tree(tree);

    return 0;
}
