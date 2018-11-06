/* Red-black tree data-structure
 * Last updated 5 November 2018 by Marcel Goh
 * Adapted from implementations found on GeeksforGeeks.com and Wikipedia
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "tree.h"

#define ERR_VAL INT_MIN
#define RED 'r'
#define BLACK 'b'
#define CHAR_NULL '\0'

/* construct a new coloured leaf containing key value pair */
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
    if (leaf->colour == BLACK) {
        printf("-");
    }
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
    if (tree->size == 0) {
        printf("()\n");
    } else {
        print_leaf_rec(tree->root);
        printf("\n");
    }
    return 0;
}

/* return sibling of a leaf, NULL if none found */
LEAF* sibling(const LEAF *leaf) {
    if (leaf == NULL) {
        printf("Passed a NULL leaf: TREE.SIBLING\n");
        return NULL;
    }
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
    if (leaf == NULL) {
        printf("Passed a NULL leaf: TREE.GRANDPARENT\n");
        return NULL;
    }
    LEAF *p = leaf->parent;
    if (p == NULL) {
        return NULL;
    } else {
        return p->parent;
    }
}

/* return uncle of leaf, NULL if none found */
LEAF* uncle(const LEAF *leaf) {
    if (leaf == NULL) {
        printf("Passed a NULL leaf: TREE.UNCLE\n");
        return NULL;
    }
    LEAF *p = leaf->parent;
    if (p == NULL) {
        return NULL;
    } else {
        return sibling(p);
    }
}

/* rotate left at leaf */
int rotate_left(TREE *tree, LEAF *leaf) {
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
    } else {
        tree->root = new;
    }
    new->parent = p;

    return 0;
}

/* rotate right at leaf */
int rotate_right(TREE* tree, LEAF *leaf) {
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
    } else {
        tree->root = new;
    }
    new->parent = p;

    return 0;
}

/* helper function to sink the leaf to the right spot in BST */
int sink(LEAF *leaf, LEAF *curr_node) {
    if (leaf == NULL || curr_node == NULL) {
        printf("One of the arguments is NULL: TREE.SINK\n");
        return ERR_VAL;
    }
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

/* takes in a tree and the node just inserted and repairs the tree */
int insert_repair(TREE *tree, LEAF *leaf) {
    LEAF *p = leaf->parent;
    if (p == NULL) {
        /* if leaf is root, change colour to black and we're done */
        leaf->colour = BLACK;
        tree->root = leaf;
    } else {
        LEAF *u = uncle(leaf);
        LEAF *g = grandparent(leaf);
        /* if leaf's parent is black we are done because leaf is red */
        if (p->colour != BLACK) {
            if (u == NULL || u->colour == BLACK) {
                /* uncle is black or null: there are FOUR cases to consider */
                if (p == g->left) {
                    if (leaf == p->right) {
                        /* CASE: leaf is left-right grand-child */
                        rotate_left(tree, p);
                        /* now it looks like left-left case (but with leaf and p flipped) */
                    }
                    /* CASE: leaf is left-left grand-child of its grandparent */
                    rotate_right(tree, g);
                } else {
                    if (leaf == p->left) {
                        /* CASE: leaf is right-left grand-child */
                        rotate_right(tree, p);
                        /* now it looks (almost) like right-right case */
                    }
                    /* CASE: leaf is right-right grand-child */
                    rotate_left(tree, g);
                }
                /* recolour grandparent and its new parent */
                g->colour = RED;
                g->parent->colour = BLACK;
                if (tree->root == g) {
                    /* if the grandparent used to be the root, set it to the new parent */
                    tree->root = g->parent;
                }
            } else {
                /* uncle is red: the structure is OK but we need to recolour the tree */
                p->colour = BLACK;
                u->colour = BLACK;
                g->colour = RED;
                /* set grandparent as leaf and recurse */
                insert_repair(tree, g);
            }
        }
    }
    return 0;
}

/* insert a new key-value pair into tree */
int insert(TREE *tree, int k, char v) {
    if (tree == NULL) {
        printf("Passed a NULL tree: TREE.INSERT\n");
        return ERR_VAL;
    }
    /* create a new leaf */
    LEAF *leaf = new_leaf(k, v, RED);
    /* perform normal BST insertion */
    if (tree->root == NULL) {
        tree->root = leaf;
    } else {
        sink(leaf, tree->root);
    }
    /* repair colours and structure of tree */
    insert_repair(tree, leaf);

    ++(tree->size);
    return 0;
}

/* recursive helper function to get leaf associated with key */
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

/* return pointer to the smallest element in subtree rooted at leaf */
LEAF* get_min(LEAF *leaf) {
    if (leaf->left == NULL) {
        return leaf;
    } else {
        return get_min(leaf->left);
    }
}

/* ########################################################################## */
/* bunch of helper functions for delete_repair (roughly following Wikipedia)
 * this is kind of ugly but I originally used a huge while loop
 * and that was a nightmare to debug!
 * the cases are in reverse order, in practice they will fall upwards from 1 to 6
 */

/* returns colour of leaf, works on NULL leaves (returns BLACK) */
char get_colour(LEAF *leaf) {
    if (leaf != NULL && leaf->colour == RED) {
        return RED;
    }
    return BLACK;
}

/* sets colour of leaf if not NULL, else it does nothing */
int set_colour(LEAF *leaf, char clr) {
    if (leaf != NULL) {
        leaf->colour = clr;
        return 0;
    }
    return 1;
}

int delete_case6(TREE *tree, LEAF *leaf) {
    LEAF *s = sibling(leaf);
    set_colour(s, get_colour(leaf->parent));
    set_colour(leaf->parent, BLACK);
    if (leaf == leaf->parent->left) {
        if (s != NULL) {
            set_colour(s->right, BLACK);
        }
        rotate_left(tree, leaf->parent);
    } else {
        if (s != NULL) {
            set_colour(s->left, BLACK);
        }
        rotate_right(tree, leaf->parent);
    }
    return 0;
}

int delete_case5(TREE *tree, LEAF *leaf) {
    LEAF *s = sibling(leaf);
    if (s != NULL && get_colour(s) == BLACK) {
        if ((leaf == leaf->parent->left) &&
            (get_colour(s->right) == BLACK) &&
            (get_colour(s->left) == RED)) {
            set_colour(s, RED);
            set_colour(s->left, BLACK);
            rotate_right(tree, s);
        } else if ((leaf == leaf->parent->right) &&
                   (get_colour(s->left) == BLACK) &&
                   (get_colour(s->right) == RED)) {
            set_colour(s, RED);
            set_colour(s->right, BLACK);
            rotate_left(tree, s);
        }
    }
    return delete_case6(tree, leaf);
}

int delete_cases34(TREE *tree, LEAF *leaf) {
    LEAF *s = sibling(leaf);
    if ((s != NULL) &&
        (get_colour(s->left) == BLACK) &&
        (get_colour(s->right) == BLACK)) {
        if (get_colour(leaf->parent) == BLACK) {
            set_colour(s, RED);
            return delete_case1(tree, leaf->parent);
        } else {
            set_colour(s, RED);
            set_colour(leaf->parent, BLACK);
            return 0;
        }
    } else {
        return delete_case5(tree, leaf);
    }
}

int delete_case2(TREE *tree, LEAF *leaf) {
    LEAF *s = sibling(leaf);
    if (get_colour(s) == RED) {
        set_colour(leaf->parent, RED);
        set_colour(s, BLACK);
        if (leaf == leaf->parent->left) {
            rotate_left(tree, leaf->parent);
        } else {
            rotate_right(tree, leaf->parent);
        }
    }
    return delete_cases34(tree, leaf);
}

int delete_case1(TREE *tree, LEAF *leaf) {
    if (leaf->parent != NULL) {
        return delete_case2(tree, leaf);
    } else {
        tree->root = leaf;
        return 0;
    }
}

/* ########################################################################## */

/* delete leaf and replace it with its child, while maintaining tree properties */
int delete_repair(TREE *tree, LEAF *leaf, LEAF *child) {
    int child_null = (child == NULL) ? 1 : 0;
    /* if child is null, make child a duplicate of leaf */
    if (child_null) {
        child = new_leaf(leaf->key, leaf->value, BLACK);
    }
    LEAF *p = leaf->parent;
    /* replace m with c */
    child->parent = p;
    if (p != NULL) {
        if (p->left == leaf) {
            p->left = child;
        } else {
            p->right = child;
        }
    } else {
        tree->root = child;
    }
    /* here if leaf is red we're actually done! */
    if (get_colour(leaf) == BLACK) {
        if (get_colour(child) == RED) {
            /* if leaf was black and now child is red, just change child to black */
            set_colour(child, BLACK);
        } else {
            /* if both leaf and child are black, start the rigmarole we wrote upstairs */
            delete_case1(tree, child);
        }
        free(leaf);
    }
    if (child_null) {
        if (child->parent != NULL) {
            if (child->parent->left == child) {
                child->parent->left = NULL;
            } else {
                child->parent->right = NULL;
            }
        } else {
            tree->root = NULL;
        }
        free(child);
    }

    return 0;
}


/* recursive method to delete leaf from tree */
int delete_leaf_rec(TREE *tree, LEAF *leaf) {
    if (leaf->left != NULL && leaf->right != NULL) {
        /* leaf has two children, so delete further down the tree */
        LEAF *right_min = get_min(leaf->right);
        leaf->key = right_min->key;
        leaf->value = right_min->value;
        return delete_leaf_rec(tree, right_min);
    } else {
        /* third argument to delete_repair call might be NULL (it's okay) */
        if (leaf->left == NULL) {
            delete_repair(tree, leaf, leaf->right);
        } else {
            delete_repair(tree, leaf, leaf->left);
        }
        return 0;
    }
}

/* deletes and returns value of leaf with given key if it exists */
char delete(TREE *tree, int k) {
    char ret_val = CHAR_NULL;
    if (tree == NULL) {
        printf("Passed a NULL tree: TREE.DELETE\n");
        return ret_val;
    }
    LEAF *leaf = search(k, tree->root);
    if (leaf != NULL) {
        ret_val = leaf->value;
        delete_leaf_rec(tree, leaf);
        --(tree->size);
    }
    return ret_val;
}
