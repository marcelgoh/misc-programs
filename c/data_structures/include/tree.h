/* Header for red-black tree data-structure in tree.c */

/* a single leaf on the tree (ALL NODES ARE CALLED LEAVES to prevent name clash) */
typedef struct leaf_s LEAF;
struct leaf_s {
    int key;
    char value;
    char colour;
    LEAF *parent;
    LEAF *left;
    LEAF *right;
};

/* a tree is a pointer to the root leaf */
typedef struct tree_s {
    LEAF *root;
    int size;
} TREE;

/* construct a new (empty) tree */
TREE* new_tree();

/* delete tree and free memory */
int free_tree(TREE **tree);

/* print leaves of tree recursively */
int print_leaf_rec(const LEAF *leaf);

/* print a tree to standard output */
int print_tree(const TREE *tree);

/* insert a new key-value pair into tree */
int insert(TREE *tree, int k, char v);

/* finds and returns value associated with key, or '\0' if not found */
char find(const TREE *tree, int k);

/* deletes and returns value of leaf with given key if it exists */
char delete(TREE *tree, int k);
