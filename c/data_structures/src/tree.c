/* Red-black tree data-structure
 * Last updated 17 October 2018 by Marcel Goh
 */

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define ERR_VAL INT_MIN
#define RED 'r'
#define BLACK 'b'
#define DOUBLE_BLACK 'd'
#define NULL_INT INT_MIN
#define NULL_CHAR '\0'

/* a single leaf on the tree (ALL NODES ARE CALLED LEAVES to prevent name clash) */
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
    print_leaf_rec(tree->root);
    printf("\n");
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
                        rotate_left(p);
                        /* now it looks like left-left case (but with leaf and p flipped) */
                    }
                    /* CASE: leaf is left-left grand-child of its grandparent */
                    rotate_right(g);
                } else {
                    if (leaf == p->left) {
                        /* CASE: leaf is right-left grand-child */
                        rotate_right(p);
                        /* now it looks (almost) like right-right case */
                    }
                    /* CASE: leaf is right-right grand-child */
                    rotate_left(g);
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

/* delete leaf m and replace it with its child c, while maintaining tree properties */
int delete_repair(TREE *tree, LEAF *m, LEAF *c) {
    int c_null = (c == NULL) ? 1 : 0;
    /* if c is null, make c a duplicate of m */
    if (c_null) {
        c = new_leaf(m->key, m->value, BLACK);
    }
    LEAF *c_duplicate = c;
    LEAF *p = m->parent;
    /* replace m with c */
    c->parent = p;
    if (p != NULL) {
        if (p->left == m) {
            p->left = c;
        } else {
            p->right = c;
        }
    } else {
        tree->root = c;
    }
    if (m->colour == RED || c->colour == RED) {
        /* Simple case: just change child's colour and we're done */
        c->colour = BLACK;
    } else {
        /* BOTH m and c are black:
         * NOTE: if c is null, we do everything on m instead and delete m after
         */
        int done = 0;
        LEAF *s;
        /* dummy while loop to allow CASE 3 to jump back to CASE 1 if needed */
        while (done == 0) {
            done = 1;
            /* CASE 1: c is the new root and we're done */
            if (c->parent == NULL) {
                printf("Here1\n");
                tree->root = c;
                break;
            }
            /* CASE 2: the sibling is red */
            s = sibling(c);
            if (s != NULL) {
                printf("%c %c\n", s->value, s->colour);
            }
            if (s != NULL && s->colour == RED) {
                printf("Here2\n");
                c->parent->colour = RED;
                s->colour = BLACK;
                if (c == c->parent->left) {
                    rotate_left(c->parent);
                } else {
                    rotate_right(c->parent);
                }
            }
            s = sibling(c);
            if ((s != NULL) &&
                (s->colour == BLACK) &&
                (s->left == NULL || s->left->colour == BLACK) &&
                (s->right == NULL || s->right->colour == BLACK)) {
                if (p->colour == BLACK) {
                    printf("Here3\n");
                    /* CASE 3: parent, sibling, and sibling's children are all black */
                    s->colour = RED;
                    c = c->parent;
                    /* restart at CASE 1 */
                    continue;
                } else {
                    /* CASE 4: sibling and its children are black, but parent is red */
                    printf("Here4\n");
                    s->colour = RED;
                    c->parent->colour = BLACK;
                    break;
                }
            }
            /* CASE 5: s is black, s's left child is red, s's right child is black, c is a left child */
            s = sibling(c);
            printf("Here5\n");
            if (s != NULL && s->colour == BLACK) {
                if ((s->right == NULL || s->right->colour == BLACK) &&
                    (s->left != NULL && s->left->colour == RED)) {
                    s->colour = RED;
                    if (c == c->parent->left) {
                        s->left->colour = BLACK;
                        rotate_right(s);
                    } else {
                        s->right->colour = BLACK;
                        rotate_left(s);
                    }
                }
            }
            /* CASE 6: s is black, s's right child is red, c is a left child */
            s = sibling(c);
            printf("Here6\n");
            if (s != NULL) {
                s->colour = c->parent->colour;
                c->parent->colour = BLACK;
                if (c == c->parent->left) {
                    if (s->right != NULL) {
                        s->right->colour = BLACK;
                    }
                    rotate_left(c->parent);
                } else {
                    if (s->left != NULL) {
                        s->left->colour = BLACK;
                    }
                    rotate_right(c->parent);
                }
            }
        }
        /* delete the duplicate if we made one */
        if (c_null) {
            if (c_duplicate->parent != NULL) {
                if (c_duplicate == c_duplicate->parent->left) {
                    c_duplicate->parent->left = NULL;
                } else {
                    c_duplicate->parent->right = NULL;
                }
            }
            free(c_duplicate);
        }
        free(m);
    }


    return 0;
}


/* recursive method to delete leaf from tree */
LEAF* delete_leaf_rec(TREE *tree, LEAF *leaf) {
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
        return leaf;
    }
}

/* deletes and returns node with given key if it exists */
LEAF* delete(TREE *tree, int k) {
    if (tree == NULL) {
        printf("Passed a NULL tree: TREE.DELETE\n");
        return NULL;
    }
    LEAF *leaf = search(k, tree->root);
    if (leaf != NULL) {
        delete_leaf_rec(tree, leaf);
        --(tree->size);
    }
    return leaf;
}

int main() {
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

    printf("%d\n", tree->size);

    return 0;
}
