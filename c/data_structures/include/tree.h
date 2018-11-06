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

/* construct a new coloured leaf containing key value pair */
LEAF* new_leaf(int k, char v, char clr);

/* construct a new (empty) tree */
TREE* new_tree();

/* recursive function to delete a leaf and all its children
 * mainly used to delete the whole tree, because delete() function below
 * does deleting a single node properly
 */
int free_leaf_rec(LEAF **leaf);

/* delete tree and free memory */
int free_tree(TREE **tree);

/* print leaves of tree recursively */
int print_leaf_rec(const LEAF *leaf);

/* print a tree to standard output */
int print_tree(const TREE *tree);

/* return sibling of a leaf, NULL if none found */
LEAF* sibling(const LEAF *leaf);

/* return grandparent of leaf, NULL if none found */
LEAF* grandparent(const LEAF *leaf);

/* return uncle of leaf, NULL if none found */
LEAF* uncle(const LEAF *leaf);

/* rotate tree left at leaf */
int rotate_left(TREE *tree, LEAF *leaf);

/* rotate tree right at leaf */
int rotate_right(TREE *tree, LEAF *leaf);

/* helper function to do normal BST insert */
int sink(LEAF *leaf, LEAF *curr_node);

/* takes in a tree and the node just inserted and repairs colours and structure */
int insert_repair(TREE *tree, LEAF *leaf);

/* insert a new key-value pair into tree */
int insert(TREE *tree, int k, char v);

/* recursive helper function to get leaf associated with key
 * runs in time proportional to the height of the tree */
LEAF* search(int k, LEAF *curr_node);

/* finds and returns value associated with key, or '\0' if not found */
char find(const TREE *tree, int k);

/* return pointer to the smallest element in subtree rooted at leaf */
LEAF* get_min(LEAF *leaf);

/* return colour of leaf, works on NULL leaves as well (returns BLACK) */
char get_colour(LEAF *leaf);

/* sets colour of leaf if not NULL, else does nothing */

/* helper functions for delete procedure */
int delete_case6(TREE *tree, LEAF *leaf);
int delete_case5(TREE *tree, LEAF *leaf);
int delete_cases34(TREE *tree, LEAF *leaf);
int delete_case2(TREE *tree, LEAF *leaf);
int delete_case1(TREE *tree, LEAF *leaf);

/* delete leaf and replace it with its child, while maintaining tree properties */
int delete_repair(TREE *tree, LEAF *leaf, LEAF *child);

/* recursive method to delete leaf from tree */
int delete_leaf_rec(TREE *tree, LEAF *leaf);

/* deletes and returns value of leaf with given key if it exists */
char delete(TREE *tree, int k);
