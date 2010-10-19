 /**************************************************************************
 * stree.c, code for generalized standard and k-truncated suffix trees    *
 * Copyright (C) <2006> Marcel Schulz, Sebastian Bauer, Peter N. Robinson *
 * This program is free software; you can redistribute it and/or modify   *
 * it under the terms of the GNU General Public License as published by   *
 * the Free Software Foundation; either version 2 of the License, or      *
 * (at your option) any later version.                                    *
 *                                                                        *
 * This program is distributed in the hope that it will be useful,        *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 * GNU General Public License for more details.                           *
 *                                                                        *
 * You should have received a copy of the GNU General Public License      *
 * along with this program; if not, write to the Free Software            *
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,                 *
 * MA  02110-1301  USA                                                    *
 * Contact: peter.robinson@charite.de                                     *
 *                                                                        *
 **************************************************************************/

#include <assert.h>
#include <string.h>  /* memset */
#include <stdlib.h>
#include <stdio.h>
#include "suffix_tree.h"


/* some assert macros */
#define ASSERT_IS_LEAF(node)			(assert((node)->isaleaf))
#define ASSERT_IS_INTERNAL_NODE(node) 	(assert(!((node)->isaleaf)))

#define USE_LINKEDLIST

 
/*****************************************************************/

#define MIN(a,b) ((a)<(b)?(a):(b))

/*****************************************************************/

/* The "trick" of renaming functions */
#ifdef SHORTINTLEAF
#define FUNCTIONNAME(x) sh ## x
typedef unsigned short int_type;
#else
#define FUNCTIONNAME(x) lo ## x
typedef unsigned int int_type;
#endif


#define DNA_SZ 4



/*****************************************************************/

typedef struct RANGE
{
  int string_index;
  int start_pos;
  struct RANGE* next;
}RANGE;


typedef struct clone
{
  int   length;
  char *string;
  int   freq;
  struct RANGE*  ranges;
  struct clone *next;
  struct clone *prev;
}CLONE;


/* Common node structure used for leaves as well as for
 * internal nodes / leaves */
struct node
{
  int isaleaf;
  char *edgestr;
  char *str_acc;
  int edgelen;
  CLONE *clone;
  struct node *parent;
  struct node *next; /* Pointer to next sibling */
};


/* Internal leaf. A simple linked list */
struct int_leaf
{
  int_type strid, pos;
  struct int_leaf *next;
};


/* Internals nodes can have children and can contain kmers (stored within internal leaves) */
struct internal_node
{
	struct node n;
	struct node *suffix_link;

	struct int_leaf *leaves;

	struct node *first; /* Pointer to first child */
};

#define INTERNAL_NODE_SET_SUFFIXLINK(node,sl) ((((struct internal_node*)node)->suffix_link) = (sl))
#define INTERNAL_NODE_GET_SUFFIXLINK(node) (((struct internal_node*)node)->suffix_link)

/* Structure for real leaves, we don't need the suffix link or children */
struct leaf_node
{
	struct node n;
	int strid, pos;
};

/* The truncated suffix tree struct differs to the normal suffix tree only
 * that it has maximum depth k, i.e. no suffix is longer than k.
 * The variable k thus only has meaning for the trucated version of
 * the suffix tree.
 */
struct stree
{
  struct item_pool *node_pool;
  struct freeable_item_pool *leaf_node_pool;
  struct item_pool *int_leaf_pool;

  struct node *root;

  int num_nodes;

  char **strings;
  int *lengths, *ids;
  int alpha_size;
  int nextslot, strsize;

  int tree_size;
  int num_compares, edges_traversed, links_traversed;
  int child_cost, nodes_created, creation_cost;

  int (*hitcallback) (int, int, void *);    /* temporary storage for the hitcallback */
  int (*hitcallback3) (int,int,int,void *); /* Alternatve hit callback with 3 int args */
  void *userdata;			    /* temporary storage for the userdata */
  int hits;				    /* temporary storage for the hit counter */
  int k;				    /* the maximum depth of the tree */
  struct clone *clones;
};



void print_clones(CLONE *clones);
int print_node(struct stree *tree, struct node *node);
/*****************************************************************/

/* intleaf */
#define intleaf_get_strid(ileaf) (ileaf->strid)

/* node prototypes */
#define node_isaleaf(n) ((n)->isaleaf)
#define node_get_edgestr(node)  ((node)->edgestr)
#define node_get_edgelen(node)  ((node)->edgelen)
#define node_get_char(node)  (*((node)->edgestr))
#define node_get_parent(node) ((node)->parent)
#define leaf_get_strid(leaf) ((leaf)->strid)

/* stree prototypes */
#define stree_get_string(tree,id)  ((tree)->strings[(id)])
#define stree_get_length(tree,id)  ((tree)->lengths[(id)])

/* Some forward declarations (prototypes) */
static struct node *stree_edge_split(struct stree *tree,
				     struct node *node,
				     int len);
static void node_reconnect(struct stree *stree, struct internal_node *parent,
                           struct node *oldchild, struct node *newchild);

void stree_delete_tree(struct stree *tree);

void debugPrintTruncatedTree(struct stree *tree);

/*****************************************************************/

/***************************************************************
 Allocate and initialize a new internal node instance

 Parameters:  tree    - A suffix tree
              edgestr - The edge label on the edge to the node.
              edgelen - The edge label's length.

 Returns:  The structure or NULL.
****************************************************************/
static struct internal_node *new_internal_node(struct stree *stree,
					       char *edgestr,
					       int_type edgelen)
{
	struct internal_node *node;

	node = item_pool_alloc(stree->node_pool);

	memset(node, 0, sizeof(*node));
	node->n.edgestr = edgestr;
	node->n.edgelen = edgelen;
	return node;
}


/***************************************************************
 Allocate and initialize a new leaf instance

 Parameters:  tree   - A suffix tree
              strid  - The id of the string containing the new
                       suffix to be added to the tree.
             edgestr - A pointer to the BEGINNING of the string
                       in which we have the  edge
             edgepos - The position of the edge label in the string.
             leafpos - The position of the new suffix in the string.

  Also, we pass the length of the edge directly. See the MACROs in
  stree.h stree_get_string and stree_get_length

  Returns:  The leaf_node structure (crashes if no memory left).
****************************************************************/
static struct leaf_node *new_leaf(struct stree *st, int_type strid, char *edgestr,  int_type edgelen, int_type leafpos)
{
 	struct leaf_node *leaf;

	leaf = freeable_item_pool_alloc(st->leaf_node_pool);

	/* We touch every member and hence we don't need to initialize the structure
	 * via memset() */
	leaf->n.isaleaf = 1;
	leaf->n.edgestr = edgestr;
 	leaf->n.edgelen = edgelen;
 	leaf->n.parent = NULL;
 	leaf->n.next = NULL;
	leaf->strid = strid;
	leaf->pos = leafpos;

	return leaf;
}

/***************************************************************
 Free the memory associated with the leaf
****************************************************************/
static void free_leaf(struct stree *st, struct leaf_node *leaf)
{
	freeable_item_pool_free(st->leaf_node_pool,leaf);
}

/***************************************************************
 Adds an intleaf initialized with the given parameters
 to the given node.

 Parameters:  stree - a truncated or standard suffix tree.
              n     -  A tree node.
              id    -  The internal identifier of the string.
              pos   -  The position of the suffix in the string.

 Returns:  Non-zero on success, zero on error.
****************************************************************/
static int node_add_intleaf(struct stree *stree, struct node *n, int_type strid, int_type pos)
{
	struct int_leaf *intleaf;
	struct internal_node *node;

	ASSERT_IS_INTERNAL_NODE(n);

	node = (struct internal_node*)n;
	/* create the new internal leaf element */
	intleaf = item_pool_alloc(stree->int_leaf_pool);
	intleaf->strid = strid;
	intleaf->pos = pos;

	/* Prepend the element  */
	intleaf->next = node->leaves;
	node->leaves = intleaf;

	return 1;
}

/***************************************************************
 Find the child of a node whose edge label begins with the
 character given as a parameter.

 Parameters:  node  -  a tree node
              ch    -  a character

 Returns:  a tree node or NULL.
****************************************************************/
struct node *node_find_child(struct node *node, char ch)
{
	if (node_isaleaf(node))
		return NULL;
	else
	{
		struct node *child;
		for (child = ((struct internal_node*)node)->first;child != NULL;child = child->next)
		{
			if (node_get_char(child) == ch)
				return child;
		}
		return NULL;
	}
}

/***************************************************************
 Convert a LEAF structure into a NODE structure and replace the
 NODE for the LEAF in the suffix tree..

 Parameters:  node  -  a leaf of the tree

 Returns:  The NODE structure corresponding to the leaf, or NULL.
****************************************************************/
static struct node *convert_leaf_into_internal_node(struct stree *stree, struct node *node)
{
	struct internal_node *newnode;
	struct leaf_node *leaf;
	struct int_leaf *ileaf;

	ASSERT_IS_LEAF(node);
	leaf = (struct leaf_node*)node;
	newnode = new_internal_node(stree,leaf->n.edgestr,leaf->n.edgelen);

	ileaf = item_pool_alloc(stree->int_leaf_pool);
	ileaf->next = NULL;
	ileaf->strid = leaf->strid;
	ileaf->pos = leaf->pos;
	newnode->leaves = ileaf;

	node_reconnect(stree,(struct internal_node*)node->parent, node, &newnode->n);
	free_leaf(stree,leaf);

	return &newnode->n;
}

/***************************************************************
 Connect a node as the child of another node.

 Parameters:  parent -  The node to get the new child.
              child  -  The child being replaced

 Returns:  The parent after the child has been connected (if the
           parent was originally a leaf, this may mean replacing
           the leaf with a node).
****************************************************************/
static struct node *node_connect(struct stree *stree, struct node *p, struct node *child)
{
	struct internal_node *parent;

	if (node_isaleaf(p))
	{
		p = convert_leaf_into_internal_node(stree,p);
	}

	child->parent = p;
	parent = (struct internal_node*)p;

	if (parent->first)
	{
		child->next = parent->first;
	}
	parent->first = child;
	return p;
}

/***************************************************************
 Disconnect the given node to_remove from its parent
****************************************************************/
static void node_disconnect(struct internal_node *parent, struct node *to_remove)
{
	struct node *child;
	struct node *prev_child = NULL;

	for (child = parent->first;child != NULL;child = child->next)
	{
		if (child == to_remove)
		{
			if (!prev_child)
				parent->first = to_remove->next;
			else
				prev_child->next = to_remove->next;

			to_remove->next = NULL;
			break;
		}
		prev_child = child;
	}
}

/***************************************************************
 Replaces one node with another in the suffix tree, reconnecting
 the link from the parent to the new node.

 Parameters:  parent    -  The parent of the node being replaced
              oldchild  -  The child being replaced
              newchild  -  The new child

 Returns:  nothing
****************************************************************/
static void node_reconnect(struct stree *stree, struct internal_node *parent,
                           struct node *oldchild, struct node *newchild)
{
	node_disconnect(parent,oldchild);
	node_connect(stree,&parent->n,newchild);
	newchild->parent = &parent->n;
	oldchild->parent = NULL;
}

/***************************************************************
 Return the root of the tree.
****************************************************************/
static struct node *stree_get_root(struct stree *tree)
{
  return tree->root;
}

/****************************************************************
 Insert a string into the list of strings maintained in the
 TRUNC_SUFFIX_TREE structure, in preparation for adding the
 suffixes of the string to the tree.

 Parameters:  tree  -  A truncated suffix tree
              S     -  The sequence
              M     -  The sequence length
              strid -  The id of the string being inserted.

 Returns:  The internal index into the TRUNC_SUFFIX_TREE structure's
           strings/lengths/ids arrays.
****************************************************************/
static int stree_insert_string(struct stree *tree, char *S,
                            int M, int strid)
{
  int slot;

  slot = tree->nextslot;
  tree->strings[slot] = S;

  tree->lengths[slot] = M;
  tree->ids[slot] = strid;

  tree->nextslot++;

  return slot;
}



static int stree_ukkonen_add_string(struct stree *tree, char *S, int M, int strid)
{
	int i, j, g, h, gprime, edgelen, id;
	char *edgestr;
	struct node *node, *lastnode, *root, *child, *parent;
	struct leaf_node *leaf;

	if ((id = stree_insert_string(tree, S, M, strid)) == -1)
		return 0;

  /*
   * Run Ukkonen's algorithm to add the string to the suffix tree.
   *
   * This implementation differs from the Gusfield book description in
   * several ways:
   *    1) The algorithm begins at the root of the tree and
   *       constructs I_{1} (the implicit suffix tree for just
   *       the first character) using the normal extension rules.
   *       The reason for this is to be able to handle generalized
   *       suffix trees, where that first character may already
   *       be in the tree. 
   *    2) The algorithm inserts the complete suffix into the
   *       tree when extension rule 2 applies (rather than deal
   *       with the business of "increasing" suffices on the leaf
   *       nodes).
   *    3) All of the offsets into the sequence, and the phases of
   *       the algorithm, use the C array indices of 0 to M-1, not 1 to M.
   *    4) The algorithm handles the conversion from implicit tree
   *       to true suffix tree by adding an additional "phase" M.  In
   *       that phase, the leaves that normally would be added because
   *       of the end of string symbol '$' are added (without resorting
   *       to the use of a special symbol).
   *    5) The constructed suffix tree only has suffix links in
   *       the internal nodes of the tree (to save space).  However,
   *       the stree_get_suffix_link function will return the suffix links
   *       even for leaves (it computes the leaves' suffix links on the
   *       fly).
   */

	root = stree_get_root(tree);
	node = lastnode = root;

	/* Represents the current offset within the edge currently chosen to be walked down.
   	 * Note that if g == 0 or edgelen we are at a node! */
	g = 0;

	edgelen = 0;
	edgestr = NULL;

	for (i=0,j=0; i <= M; i++)
	{
		for ( ; j <= i && j < M; j++)
		{
			/* Perform the extension from S[j..i-1] to S[j..i].  One of the
			 * following two cases holds:
			 *    a) g == 0, node == root and i == j.
			 *         (meaning that in the previous outer loop,
			 *          all of the extensions S[1..i-1], S[2..i-1], ...,
		     *          S[i-1..i-1] were done.)
		     *    b) g > 0, node != root and the string S[j..i-1]
		     *       ends at the g'th character of node's edge. */
			if (g == 0 || g == edgelen)
			{
				if (i < M)
				{
					/* If an outgoing edge matches the next character, move down
				     * that edge. */
					if ((child = node_find_child(node, S[i])) != NULL)
					{
						node = child;
						g = 1;
						edgestr = node_get_edgestr(node);
						edgelen = node_get_edgelen(node);
						break;
					}

					/* Otherwise, add a new leaf out of the current node. */
					leaf = new_leaf(tree, id, stree_get_string(tree,id)+i, stree_get_length(tree,id)-i, j);
					node = node_connect(tree, node, (struct node*) leaf);
					tree->num_nodes++;
				} else
				{
					/* If i == M, then the suffix ends inside the tree, so
				     * add a new intleaf at the current node. */
           			if (node_isaleaf (node)) node = convert_leaf_into_internal_node(tree, node);
					if (!node_add_intleaf(tree, node, id, j)) return 0;
				}

				ASSERT_IS_INTERNAL_NODE(lastnode);
				if (lastnode != root && !INTERNAL_NODE_GET_SUFFIXLINK(lastnode))
					INTERNAL_NODE_SET_SUFFIXLINK(lastnode,node);
				lastnode = node;
			} else /* g != 0 */
			{
				/* If the next character in the edge label matches the next
				 * input character, keep moving down that edge (i.e. do nothing
				 * within this extension). */

				if (i < M && S[i] == edgestr[g])
				{
					g++;
					break;
				}

				/* Otherwise, split the edge at that point and add a new leaf for the suffix. */
				if ((node = stree_edge_split (tree, node, g)) == NULL)
					return 0;

				edgestr = node_get_edgestr(node);
				edgelen = node_get_edgelen(node);

				if (i < M)
				{
					leaf = new_leaf(tree, id, stree_get_string(tree,id)+i, stree_get_length(tree,id)-i, j);
					node = node_connect(tree, node, (struct node*) leaf);
					tree->num_nodes++;
				} else
				{
					/* If i == M, then the suffix ends inside the tree, so
					 * add a new intleaf at the node created by the edge split. */
					if (node_isaleaf (node)) node = convert_leaf_into_internal_node(tree, node);
					if (!node_add_intleaf(tree, node, id, j)) return 0;
				}
				ASSERT_IS_INTERNAL_NODE(lastnode);
				if (lastnode != root && !INTERNAL_NODE_GET_SUFFIXLINK(lastnode))
					INTERNAL_NODE_SET_SUFFIXLINK(lastnode,node);
				lastnode = node;
			}

			/* Now, having extended S[j..i-1] to S[j..i] by rule 2, find where
			 * S[j+1..i-1] is.  Note that the values of node and g have not
			 * changed in the above code (since stree_edge_split splits the
			 * node on the g'th character), so either g == 0 and node == root
			 * or the string S[j..i-1] ends at the g-1'th character of node's
			 * edge (and node is not the root). */
			if (node == root);
			else if (g == edgelen && INTERNAL_NODE_GET_SUFFIXLINK(node) != NULL)
			{
				node = INTERNAL_NODE_GET_SUFFIXLINK(node);
				edgestr = node_get_edgestr(node);
				edgelen = node_get_edgelen(node);
				g = edgelen;
				//continue;
			} else
			{
				/* Move across the suffix link of the parent (unless the
				 * parent is the root). */
        			parent = node_get_parent(node);
				if (parent != root)
					node = INTERNAL_NODE_GET_SUFFIXLINK(parent);
				else
				{
					node = root;
					g--;
				}
				edgelen = node_get_edgelen(node);

				/* Use the skip/count trick to move g characters down the tree. */
				h = i - g;

				while (g > 0)
				{
					node = node_find_child(node, S[h]);
					gprime = node_get_edgelen(node);
					if (gprime > g)
						break;

					g -= gprime;
					h += gprime;
				}

				edgestr = node_get_edgestr(node);

				edgelen = node_get_edgelen(node);

				/* After the walk down, either "g > 0" and S[j+1..i-1] ends g
				 * characters down the edge to `node', or "g == 0" and S[j+1..i-1]
				 * really ends at `node' (i.e., all of the characters on the edge
				 * label to `node' match the end of S[j+1..i-1]).
				 *
				 * If "g > 0" or "g == 0" but `node' points to a leaf (which could
				 * happen if S[j+1..i-1] was the suffix of a previously added
				 * string), then we must delay the setting of the suffix link
				 * until a node has been created.  (With the suffix tree data
				 * structure, no suffix links can safely point to leaves of the
				 * tree because a leaf may be converted into a node at some future
				 * time.) */

				if (g == 0)
				{
					if (lastnode != root &&
					    !node_isaleaf(node) &&
					    INTERNAL_NODE_GET_SUFFIXLINK(lastnode) == NULL)
					{
						INTERNAL_NODE_SET_SUFFIXLINK(lastnode,node);
						lastnode = node;
					}

					if (node != root)
						g = edgelen;
				}
			}
		}
	}
	return 1;
}



/* static void kTstree_traverse_subtree(struct stree *tree, struct node *node, */
/*                             int (*preorder_fn)(), int (*postorder_fn)()); */


/***********************************************************
 Allocates a new truncated suffix tree data structure.

 Returns:  A TRUNC_SUFFIX_TREE structure
***********************************************************/
static struct stree *kTstree_new_tree(int number_of_strings,
				      int alphabet_size, int k)
{
	struct stree *tree;
	int i;

	tree = gsuffix_malloc(sizeof(*tree));
	memset(tree, 0, sizeof(*tree));

	tree->node_pool = item_pool_create(sizeof(struct internal_node));
	tree->int_leaf_pool = item_pool_create(sizeof(struct int_leaf));
	tree->leaf_node_pool = freeable_item_pool_create(sizeof(struct leaf_node));

	tree->k = k;
	tree->alpha_size = alphabet_size;

	tree->strsize = number_of_strings;
	tree->strings = gsuffix_malloc(tree->strsize * sizeof(char *));
	tree->lengths = gsuffix_malloc(tree->strsize * sizeof(int));
	tree->ids = gsuffix_malloc(tree->strsize * sizeof(int));

	for (i = 0; i < tree->strsize; i++)
	{
		tree->strings[i] = NULL;
		tree->lengths[i] = tree->ids[i] = 0;
	}

	tree->nextslot = 0;			/* Where to put the next string? */
	tree->root = &new_internal_node(tree, NULL, 0)->n;
	tree->num_nodes = 1;		/* just the root */
	return tree;
}


/***********************************************************
 * Allocates a standard generalized suffix tree datastructure.
 * A standard suffix tree has no need of the variable k. Otherwise
 * the structure is identical. Set k to -1, and pass work onto
 * the corresponding kTstree function.
 *************************************************************/
static struct stree *stree_new_tree(int number_of_strings,int alphabet_size)
{
  return kTstree_new_tree(number_of_strings,alphabet_size,-1);
}



/***********************************************************
 Splits an edge of the truncated suffix tree, and adds a new
 node between two existing nodes at that split point.

 Parameters:  tree  -  The truncated suffix tree
			  node  -  The tree node just below the split.
              len   -  How far down node's edge label the
                       split is.
 Return:  The new node added at the split.
***********************************************************/
static struct node *stree_edge_split(struct stree *tree,
				       struct node *node, int len)
{
  struct internal_node *newnode, *parent;
  if (node == stree_get_root(tree) ||
      len == 0 || node_get_edgelen(node) <= len)
    return NULL;

  newnode = new_internal_node(tree, node->edgestr, len);
  if (newnode == NULL)
    return NULL;

  parent = (struct internal_node*)node_get_parent(node);
  node_reconnect(tree, parent, node, &newnode->n);

  node->edgestr += len;
  node->edgelen -= len;

  if (node_connect(tree, &newnode->n, node) == NULL)
  {
  	/* TODO: This cannot happen in our implementation */
    fprintf(stderr,"Error in reconnecting node at %s (%d), terminating program.\n",
	    __FILE__,__LINE__);
    /*
      
    node->edgestr -= len;
    node->edgelen += len;
    node_reconnect(tree, (struct internal_node*)parent, &newnode->n, node);
      free_node(newnode);
  
    return NULL;
    */
  }

  /* tree->num_nodes++; */
  return &newnode->n;
}






/***********************************************************
 Traverses the suffix link from a node, and returns the node
 at the end of the suffix link.

 Parameters:  tree     -  A truncated suffix tree
              node     -  The starting node of the walk
              pos      -  The starting point on the node's edge.
              T        -  The string to match
              N        -  The matching string's length
              node_out - Where the walk ends
              pos_out  - Where on the node's edge does the walk end.

 Return:  The number of characters matched during the walk.
***********************************************************/
static int kTstree_walk_to_leaf(struct stree *tree, struct node *node, int pos,
                           const char *T, int N, struct node **node_out, int *pos_out)
{
	int len, edgelen;
	char *edgestr;
	struct node *child;
	if (node_isaleaf(node))
	{
		*node_out = node;
		*pos_out = pos;
		return 0;
	}

	edgestr = node_get_edgestr(node);
	edgelen = node_get_edgelen(node);
	len = 0;
	while (1)
	{
	  while (len < N && pos < edgelen && T[len] == edgestr[pos])
	    {
	      pos++;
	      len++;
	    }
	  
	  if (len == N || pos < edgelen || (child = node_find_child(node, T[len])) == NULL) {
	    break;
	  }
	  
	  if (node_isaleaf(child))
	    {
	      print_node(NULL,child);
	      *node_out = child;
	      *pos_out = 0; 
	      return len;
	    }
	  
	  node = child;
	  edgestr = node_get_edgestr(node);
	  edgelen = node_get_edgelen(node);
	  pos = 1;
	  len++;
	}
	
	*node_out = node;
	*pos_out = pos;
	return len;
}

/***********************************************************
 ...
***********************************************************/
static int kTstree_walk(struct stree *tree, struct node *node,
			int pos, const char *T, int N,
               struct node **node_out, int *pos_out)
{
  int len, endpos, edgelen;
  char *edgestr;
  struct node *endnode;

  len = kTstree_walk_to_leaf(tree, node, pos, T, N, &endnode, &endpos);

  /* find the match; commented by HL */
  if (!node_isaleaf(endnode) || len == N)
  {
    *node_out = endnode;
    *pos_out = endpos;
    return len;
  }

  edgestr = node_get_edgestr(endnode);
  edgelen = node_get_edgelen(endnode);
  /* match ends half way through the edge string;; commented by HL */
  while (len < N && endpos < edgelen && T[len] == edgestr[endpos])
  {
    len++;
    endpos++;
  }

  *node_out = endnode;
  *pos_out = endpos;
  return len;
}




/***********************************************************
 Traverse the path down the tree whose path label matches T, and return
 the number of characters of T matches, and the node and position along
 the node's edge where the matching to T ends.

 Parameters:  tree      -  a truncated suffix tree
              node      -  what node to start the walk down the tree
              pos       -  position along node's edge to start the walk
                              (`node' and `pos' are kTstree_walk only)
              T         -  the sequence to match
              N         -  the sequence length
              node_out  -  address of where to store the node where
                           the traversal ends
              pos_out   -  address of where to store the character position
                           along the ending node's edge of the endpoint of
                           the traversal

 Returns:  The number of characters of T matched.
************************************************************/
static int kTstree_match(struct stree *tree, const char *T, int N,
                struct node **node_out, int *pos_out)
{
  return kTstree_walk(tree, stree_get_root(tree), 0, T, N, node_out, pos_out);
}


/***********************************************************
 Return the number of children of a node.

 Parameters:  tree  -  a truncated suffix tree
              node  -  a tree node

 Returns:  the number of children.
************************************************************/
static int kTstree_get_num_children(struct stree *tree, struct node *node)
{

  int count;
  struct node *child;

  if (node_isaleaf(node)) return 0;

  for (child = ((struct internal_node*)node)->first,count=0;child != NULL;child = child->next)
	count++;

  return count;
  
}

/*
 * kTstree_traverse & kTstree_traverse_subtree
 *
 * Use a non-recursive traversal of the tree (or a subtree), calling the
 * two function parameters before and after recursing at each node, resp.
 * When memory is at a premium, this traversal may be useful.
 *
 * Note that either of the function parameters can be NULL, if you just
 * need to do pre-order or post-order processing.
 *
 * The traversal uses the `isaleaf' field of the tree nodes to hold its
 * state information.  Since `isaleaf' will always be 0 at the internal
 * nodes where the state information is needed, this will not affect the
 * values at the nodes (and all changes to `isaleaf' will be undone.
 *
 * Parameters:  tree          -  a truncated suffix tree
 *              node          -  root node of the traversal
 *                                 (kTstree_traverse_subtree only)
 *              preorder_fn   -  function to call before visiting the children
 *              postorder_fn  -  function to call after visiting all children
 *
 * Returns:  nothing.
 */

/*******************************************************
 Looks up a given p with length m. On every hit,
 hit_callback is called with arguments "index" and "pos"
 The index argument determines the string index as upon
 creation of the genarray and the argument "pos"
 determines the position of the hit within that string.

 Returns the number of hits.

 Note this function can be called eighter using a suffix
 tree or a trunctated suffix tree.
 ********************************************************* */
/* int gsuffix_lookup(struct stree *tree, const char *pattern, int pattern_length, int (*hitcallback)(int index, int pos, void *userdata), void *userdata) */
/* { */
/* 	int pos,matchlen; */
/* 	struct node *node; */
/* 	char *mapped_pattern; */

/* #ifndef USE_LINKEDLIST */
/* 	char buf[16]; */
/* #endif */

/* 	/\* store hitcallback function and the hits in the truncated suffix tree *\/ */
/* 	tree->hitcallback = hitcallback; */
/* 	tree->userdata = userdata; */
/* 	tree->hits = 0; */

/* #ifndef USE_LINKEDLIST */
/* 	/\* For patterns of small size we rather use a local buffer than allocating */
/* 	 * new memory *\/ */
/* 	if (pattern_length < sizeof(buf)) mapped_pattern = buf; */
/* 	else mapped_pattern = gsuffix_malloc(pattern_length*sizeof(char)); */
/* 	memcpy(mapped_pattern,pattern,pattern_length); */
/* 	map_string(mapped_pattern,pattern_length); */
/* #else */
/* 	mapped_pattern = (char*)pattern; */
/* #endif */

/* 	node = stree_get_root(tree); */

/* 	/\* Test whether the string matches any edge of the truncated suffix tree,i.e. */
/* 	 * is present in the truncated suffix tree *\/ */
/* 	matchlen = kTstree_match(tree, mapped_pattern, pattern_length, &node, &pos); */
/*         printf("MatchLen: %d\n", matchlen); */
/* 	if (matchlen == pattern_length) */
/* 		kTstree_traverse_subtree(tree, node, add_match, (int (*)()) NULL); */

/* #ifndef USE_LINKEDLIST */
/* 	/\* Free the pattern if it was allocated above *\/ */
/* 	if (pattern_length >= sizeof(buf)) */
/* 		gsuffix_free(mapped_pattern); */
/* #endif */
/* 	printf("Match Hits: %d\n", tree->hits); */
/* 	return tree->hits; */
/* } */



/*******************************************************
 Looks up a given p with length m.

 Returns 1 if the pattern exists in
 the tree or 0 otherwise.

 Note this function can be called either using a suffix
 tree or a trunctated suffix tree.(it is used to compare
 wotd against gsuffix)
*********************************************************/
int gsuffix_lookup_exists(struct stree *tree, const char *pattern, int pattern_length)
{
	int pos,matchlen;
	struct node *node;
	char *mapped_pattern;

#ifndef USE_LINKEDLIST
	char buf[16];
#endif

#ifndef USE_LINKEDLIST
	/* For patterns of small size we rather use a local buffer than allocating
	 * new memory */
	if (pattern_length < sizeof(buf)) mapped_pattern = buf;
	else mapped_pattern = gsuffix_malloc(pattern_length*sizeof(char));
	memcpy(mapped_pattern,pattern,pattern_length);
	map_string(mapped_pattern,pattern_length);
#else
	mapped_pattern = (char*)pattern;
#endif

	node = stree_get_root(tree);

	/* Test whether the string matches any edge of the truncated suffix tree,i.e.
	 * is present in the truncated suffix tree */
	matchlen = kTstree_match(tree, mapped_pattern, pattern_length, &node, &pos);

   #ifndef USE_LINKEDLIST
	/* Free the pattern if it was allocated above */
	if (pattern_length >= sizeof(buf))
		gsuffix_free(mapped_pattern);
#endif

    /* if the whole pattern could be matched the pattern exists */
	if (matchlen == pattern_length)
		return 1;
	else
		return 0;

}


/***********************************************************
 Create the stree from ASCII string. Does mapping and so on.
 Parameters:
	 strings:  array of C strings
	 nstrings: number of strings in array.
***********************************************************/
struct stree *stree_create_from_char (char **strings, int nstrings)
{
	int i,len;
	char *str;
	struct stree *tree;

	if (!(tree = stree_new_tree(nstrings,DNA_SZ)))
		return NULL;

	for (i = 0; i < nstrings; ++i)
	{
		len = strlen(strings[i]);

#ifndef USE_LINKEDLIST
		str = create_mapped_string(strings[i], len);
#else
		str = strings[i];
#endif
		//	printf("\nstring to add: %s\n", str);
		if (stree_ukkonen_add_string(tree, str, len, i+1) < 1)
		{
		  stree_delete_tree(tree);
		  return NULL;
		}
	}
	/*	debugPrintTruncatedTree(tree);*/
	return tree;
}

/****************************************************
 Free all memory associated with the given tree
*****************************************************/
void stree_delete_tree(struct stree *tree)
{
	item_pool_delete(tree->node_pool);
	item_pool_delete(tree->int_leaf_pool);
	freeable_item_pool_delete(tree->leaf_node_pool);
#ifndef USE_LINKEDLIST
	{
		int i;
		for (i = 0; i < tree->strsize; i++)
			gsuffix_free(tree->strings[i]);
	}
#endif
	gsuffix_free(tree->strings);
	gsuffix_free(tree->lengths);
	gsuffix_free(tree->ids);
	gsuffix_free(tree);
}



/************************************************************
 Does the same like malloc(), but exits when failing and
 tracks memory
*************************************************************/

static int memallocated;
static int maxmem;

void *gsuffix_malloc(int size)
{
	int *mem = malloc(size+sizeof(int));
	if (!mem)
	{
		printf("Could not allocate %d bytes of memory! Aborting\n",size+4);
		exit(20);
	}
	mem[0] = size;
	memallocated += size;
	if (memallocated > maxmem) maxmem = memallocated;
	return mem+1;
}

/************************************************************
 Frees the memory which must have been allocated with
 gsuffix_malloc()
*************************************************************/
void gsuffix_free(void *mem)
{
	int *m = (int*)mem;
	if (!m) return;

	memallocated -= m[-1];
	free(m-1);
}


/***************************************************************************/

/* This is a basic implementation of an item pool. It's a quite restricted one
 * because it is optimized for speed. Once allocated, an item cannot be freed
 * individually. You can only free all items at once */

struct mypage
{
	struct mypage *next;	/* embedded node structure */

	int size;				/* number of bytes covered by the page */
	char *mem;				/* address of the page covered by the page */

	char *next_item;		/* pointer to next item */
	int items_left;			/* how many items are left within this page? */
};

struct item_pool
{
	struct mypage *head;
	int item_size;
};

/************************************************
 Instanciate a new item pool. The paramenter
 item_size specifies the number size for every
 individual item.
*************************************************/
struct item_pool *item_pool_create(int item_size)
{
	struct item_pool *ip = gsuffix_malloc(sizeof(*ip));

	ip->head = NULL;
	ip->item_size = (item_size + 3)/4*4;

	return ip;
}

/************************************************
 Free the complete item pool
*************************************************/
void item_pool_delete(struct item_pool *ip)
{
	struct mypage *p = ip->head;
	while (p)
	{
		struct mypage *next;

		next = p->next;
		gsuffix_free(p->mem);
		gsuffix_free(p);
		p = next;
	}
	gsuffix_free(ip);
}


/**************************************************
 Allocate a new page
***************************************************/
static struct mypage *item_pool_alloc_page(int item_size, int page_size)
{
	struct mypage *p;

	p = gsuffix_malloc(sizeof(*p));
	p->mem = p->next_item = gsuffix_malloc(page_size);
	p->size = page_size;
	p->next = NULL;
	p->items_left = page_size / item_size;

	return p;
}

/**************************************************
 Allocate a new item from the given item pool. May
 return NULL on failure.
***************************************************/
void *item_pool_alloc(struct item_pool *pool)
{
	char *item;

	if (!pool->head || pool->head->items_left == 0)
	{
		struct mypage *p = item_pool_alloc_page(pool->item_size,32768);
		p->next = pool->head;
		pool->head = p;
	}

	item = pool->head->next_item;
	pool->head->next_item += pool->item_size;
	pool->head->items_left--;
	return item;
}

/**************************************************************/

/* Unlike the item pool implementation above, this one allows the freeing of
 * items but requires 4 bytes of extra storage */

struct mypage2
{
	struct mypage2 *next;	/* embedded node structure */

	int size;				/* number of bytes covered by the page */
	void *mem;				/* address of the page covered by the page */

	int nitems;
};

struct item
{
	struct item *next;
};

struct freeable_item_pool
{
	struct mypage2 *page_head;
	struct item *items_head;
	int item_size;
};

/************************************************
 Instanciate a new item pool. The paramenter
 item_size specifies the number size for every
 individual item.
*************************************************/
struct freeable_item_pool *freeable_item_pool_create(int item_size)
{
	struct freeable_item_pool *ip = gsuffix_malloc(sizeof(*ip));

	ip->page_head = NULL;
	ip->items_head = NULL;
	ip->item_size = (item_size + 7)/4*4;

	return ip;
}

/************************************************
 Free the complete item pool
*************************************************/
void freeable_item_pool_delete(struct freeable_item_pool *ip)
{
	struct mypage2 *p = ip->page_head;
	while (p)
	{
		struct mypage2 *next;

		next = p->next;
		gsuffix_free(p->mem);
		gsuffix_free(p);
		p = next;
	}
	gsuffix_free(ip);
}


/**************************************************
 Allocate a new page
***************************************************/
static struct mypage2 *freeable_item_pool_alloc_page(int item_size, int page_size)
{
	struct mypage2 *p;

	p = gsuffix_malloc(sizeof(*p));
	p->mem = gsuffix_malloc(page_size);
	p->size = page_size;
	p->next = NULL;
	p->nitems = page_size / item_size;
	return p;
}

/**************************************************
 Allocate a new item from the given item pool. May
 return NULL on failure.
***************************************************/
void *freeable_item_pool_alloc(struct freeable_item_pool *pool)
{
	struct item *item;

	if (!pool->items_head)
	{
		/* Create a new page and add all items to the free list */
		struct mypage2 *p;
		int i;

		p = freeable_item_pool_alloc_page(pool->item_size,32768);
		item = pool->items_head = p->mem;

		for (i=0;i < p->nitems - 1;i++)
		{
			struct item *next_item = (struct item*)(((unsigned char*)item) + pool->item_size);
			item->next = next_item;
			item = next_item;
		}
		item->next = NULL;

		p->next = pool->page_head;
		pool->page_head = p;
	}

	item = pool->items_head;
	pool->items_head = item->next;

	return ((unsigned char*)item)+4;
}

/**************************************************
 Give back the given item to the given pool
***************************************************/
void freeable_item_pool_free(struct freeable_item_pool *pool, void *mem)
{
	struct item *item = (struct item*)(((unsigned char*)mem) - 4);
	item->next = pool->items_head;
	pool->items_head = item;
}






/**************************************************************************
 * 		DEBUGGING
 **************************************************************************
 *
 * debugPrintTree
 *
 * print out tree values and the root values
 ********************************************/


/* debugPrintNode */

/* print out node values */
void debugPrintNode(struct node *node, int depth)
{
	struct internal_node *node1; 
	struct leaf_node *leaf; 
	struct int_leaf *ileaf;

	if (!node) return;

	if (depth  == 0)
	{
		printf("root\n");
	}
	printf("%s l. %d: debugPrintNode\n",__FILE__,__LINE__);
	printf("depth: %d,\t isaleaf:\t%d;\n",depth,node_isaleaf(node));
	printf("\tedgestr: %s\t\n",node->edgestr);
	printf("\tedgelen:%d\n",node->edgelen);
	if (!node_isaleaf(node)){
	  node1 = (struct internal_node*)node;
	  printf("Not a leaf node\n");
	  if (node1->leaves != NULL){
	        printf("leaves not NULL \n");
		int num;
		num = kTstree_get_num_children(NULL, node);
		printf("Num of children %d\n", num);
	  	printf("depth %d ,nodeedgelen: %d, char %c (ILEAF)",depth,node->edgelen,*(node->edgestr)+48);
	  	ileaf = node1->leaves;
	  	while(ileaf!=NULL){
	  	  printf(" id: %d, pos: %d\t", ileaf->strid ,ileaf->pos);
	  	  ileaf = ileaf->next;
	  	}
		printf("\n");
	  }
	  debugPrintNode(node_find_child(node, 'A'),depth+1);
	  
	  debugPrintNode(node_find_child(node, 'T'),depth+1);
	  
	  debugPrintNode(node_find_child(node, '$'),depth+1);  

	  /* debugPrintNode(node_find_child(node, 'd'),depth+1);  */
	  
	  
	}
	else{
	  leaf = (struct leaf_node *) node;
	  printf("depth: %d, strid: %d, pos: %d, char %c ,edgelen: %d (LEAF)\n",depth,leaf->strid,leaf->pos,*(node->edgestr)+48,node->edgelen);
	}
}

void debugPrintTruncatedTree(struct stree *tree)
{       int i;
	printf("%s l. %d: debugPrintTree\n",__FILE__,__LINE__);
	printf("\tnum_nodes:\t\t%d\n",tree->num_nodes);
	printf("\troot node:\n*****\n");
	debugPrintNode(tree->root,0);
	printf("*****\n");
	printf("\ttree_size:%d\n",tree->tree_size);
	printf("\tstrsize:%d\n",tree->strsize);
	printf("\tstrings entered in tree structure:\n");
	for (i=0;i<tree->nextslot;++i) {
		printf("\t\tstring %d: %s\n", i ,tree->strings[i]);
	}
	printf("\tnextslot: %d\n",tree->nextslot);
}


/* static int pint_node(struct stree *tree, struct node *node) */
/* { */
/*   struct leaf_node *leaf; */
/*   struct int_leaf *intleaf; */
/*   int pos, id; */

/*   /\* if node is a leaf there can be only one match*\/ */
/*   if (node_isaleaf(node)) */
/*     { */
/*       leaf = (struct leaf_node *) node; */
/*       printf("depth: %d, strid: %d, pos: %d, char %c ,edgelen: %d (LEAF)\n",depth,leaf->strid,leaf->pos,*(node->edgestr)+48,node->edgelen); */
/*     } */
/*   else */
/*     { */
      
/*     } */
/*       leaf = (struct leaf_node*)node; */
/*       pos = leaf->pos; */

/*       id = leaf_get_strid(leaf); */
/*       tree->hits++; */

/*       return tree->hitcallback(id, pos, tree->userdata); */
/*     }  else */
/*     { */
/*       /\* traverse the linked list of intleafs and report every match *\/ */
/*       intleaf = ((struct internal_node*)node)->leaves; */

/*       for (;intleaf != NULL;intleaf=intleaf->next) */
/* 	{ */
/* 	  pos = intleaf->pos; */
/* 	  id = leaf_get_strid(intleaf); */

/* 	  tree->hits++; */
/* 	  if (!tree->hitcallback(id, pos, tree->userdata)) */
/* 	    return 0; */
/* 	} */
/*     } */
/*   return 1; */
/* } */


int print_node(struct stree *tree, struct node *node)
{
  struct internal_node *node1;
  struct leaf_node *leaf;
  struct int_leaf *ileaf;
  int num;
  /* if node is a leaf there can be only one match*/
  num = kTstree_get_num_children(tree, node);
  printf("\nNumber of children %d\n", num);
  if (node_isaleaf(node))
    {
      printf(" Is a  leaf node\n");
      leaf = (struct leaf_node *) node;
      printf("strid: %d, pos: %d, edgestr: %s edgelen: %d (LEAF)\n",leaf->strid+1,leaf->pos+1, node->edgestr, node->edgelen);
    }
  else
    {
      node1 = (struct internal_node*)node;
      printf("\nNot a leaf node\n");
      if (node1->leaves != NULL){
	printf("leaves not NULL \n");
	printf("node_edge_len: %d, edgestr: %s",node->edgelen, node->edgestr);
	ileaf = node1->leaves;
	while(ileaf!=NULL){
	  printf(" id: %d, pos: %d\t", ileaf->strid+1 ,ileaf->pos+1);
	  ileaf = ileaf->next;
	}
	printf("\n");
      }
      else
	{
	  printf("leaves NULL\n");
	  printf("node_edge_len: %d, edgestr: %s \n ",node->edgelen, node->edgestr);
	}
    }
  return 1;
}

/* void stree_print(struct stree *tree) */
/* { */
/*   kTstree_traverse(tree, (int (*)()) NULL, print_node); */
/* } */

void print_clones(CLONE *clones)
{
  struct clone *temp;
  struct RANGE* r;
  if (clones==NULL) 
    return;
  temp = clones;
  while(temp!=NULL)
    {
      printf("\n{clone string: %s\n", temp->string); 
      printf("clone length: %d;",temp->length);
      printf("clone freq: %d}:", temp->freq);
      for(r=temp->ranges; r!=NULL; r=r->next)
	printf("{strid:%d, pos: %d} ", r->string_index, r->start_pos);
      temp = temp->next;
    }
}


void print_a_clone_to_file(FILE *fp, CLONE *clone)
{
  struct RANGE* r;
  fprintf(fp, "{");
  fprintf(fp, "[");
  r = clone->ranges;
  if (r != NULL) 
    {
      fprintf(fp, "{%d,%d}", r->string_index, r->start_pos);
      r = r-> next;
      while(r!=NULL)
	{
	  fprintf(fp, ",{%d,%d}", r->string_index, r->start_pos);
	  r = r-> next;
	}
      fprintf(fp, "],%d,%d}.", clone->length, clone->freq);
    }
}

void print_clones_to_file(char *filename, CLONE *clones) 
{
  FILE *fp;
  struct clone *temp;
  struct RANGE* r;
  int first=1;
  fp = fopen((const char*)filename, "w");
  temp = clones;
  if (temp==NULL)
    {
      fprintf(fp, "[].");
      fclose(fp);
      return;
    }
  fprintf(fp, "[");
  
  while(temp!=NULL)
    {
      if (first==1)
	{
	  first=0;
	  fprintf(fp, "{");
	}
      else 
	fprintf(fp, ",{");
      r = temp->ranges;
      fprintf(fp, "[");
      if (r != NULL) 
	{
	  fprintf(fp, "{%d,%d}", r->string_index, r->start_pos);
	  r = r-> next;
	  while(r!=NULL)
	    {
	      fprintf(fp, ",{%d,%d}", r->string_index, r->start_pos);
	      r = r-> next;
	    }
	  fprintf(fp, "],%d,%d}", temp->length, temp->freq);
	}
      temp = temp->next;
    }
  fprintf(fp, "].");
  fclose(fp); 
}

void free_child_clones(struct internal_node *node) 
{
  struct node *temp;
  struct RANGE* head_range, *temp_range;
  CLONE *clone;
  for (temp =node->first; temp!=NULL; temp=temp->next)
    { 
      clone = temp-> clone;
      head_range= clone->ranges;
      while(head_range!=NULL) 
	{
	  temp_range = head_range-> next;
	  free(head_range);
	  head_range=temp_range;
	}
      free(clone->string);
      free(clone);
    }
  return;
}
 

int num_of_exprs_in_str(char* clone_str)
{
  int num=0;
  char *tl;
  char str[2048]="\0";
  strcpy(str, clone_str);
  /* printf("\n clinesss: %s\n", clone_str); */
  for (tl=strtok(str, ",$"); tl!=NULL;
       tl=strtok(NULL, ",$"))
    
    num++;
  /* printf("\n num: %d\n", num); */
  return num;
}

void free_a_clone_node(struct clone *clone)
{
  struct RANGE *temp_range, *head_range;
  head_range= clone->ranges;
  while(head_range!=NULL) 
    {
      temp_range = head_range-> next;
      free(head_range);
      head_range=temp_range;
    }
  free(clone);
}

void free_clone_nodes(struct clone *clone_head)
{
  struct clone *temp;
  while(clone_head!=NULL)
    {
      temp = clone_head->next;
      clone_head->next=NULL;
      free_a_clone_node(clone_head);
      clone_head=temp;
    }
}

/*
  return 1 if range1 is sub-ranges of ranges2;
  otherwise return 0;
*/
int sub_ranges(struct RANGE *ranges1, struct RANGE *ranges2)
{

  int found=0;
  struct RANGE *temp;
  while(ranges1!=NULL)
    {
      found = 0;
      /* printf("Temp is null: %d", ranges1==NULL); */
      temp = ranges2;
      while((temp!=NULL) && (found==0))
	{
	  /* printf("temp string index: %d;", temp->string_index); */
	  /* printf("temp start pos: %d\n", temp->start_pos); */
	  /* printf("ranges string index: %d;", ranges1->string_index); */
	  /* printf("rangs1 start pos: %d\n", ranges1->start_pos); */
	  if ((temp->string_index ==ranges1->string_index) && (temp->start_pos < ranges1->start_pos))
	    found=1;
	  else
	    temp = temp->next;
	}
      /* printf("\n found %d\n", found); */
      if (found==0)
	return 0;
      else
	ranges1=ranges1->next;
    }
  if (ranges1==NULL)
    return 1;
  else
    return 0;
}

/* return 1 if clone1 is a sub-clone of clone2;
   return 2 if clone2 is a sub-clone of clone1;
   return 0 otherwise
*/

int sub_clone(struct clone *clone1, struct clone *clone2) 
{
  if ((clone2->length>= clone1->length) && (clone2->freq>= clone1->freq))
    {
      /* printf("check sub ranges1\n"); */
      if (sub_ranges(clone1->ranges, clone2->ranges)==1)
	return 1;
      else
	return 0;
    }
  else
    if ((clone1->length>= clone2->length) && (clone1->freq>= clone2->freq))
      {
	/* printf("check sub ranges2\n"); */
	if (sub_ranges(clone2->ranges, clone1->ranges)==1)
	  return 2;
	else
	  return 0;
      }
  return 0;
}


void map_a_clone(struct clone* clone, char** seq_array, int  minlen, int minfreq)
{
  struct RANGE* r;
  int strid;
  int start_pos;
  char str[4096]="\0";
  int num_of_prev_exprs;
  char prev_str[4096]="\0";
  char clone_str[4096]="\0";
  int i;
  r=clone->ranges;
  strid = r->string_index-1;
  start_pos = r->start_pos;
  strcpy(str, seq_array[strid]);
  strncpy(prev_str, str, start_pos-1);
  prev_str[start_pos-1]='\0';
  for (i=0; i<clone->length; i++)
    clone_str[i]=str[start_pos+i-1];
  clone_str[clone->length]='\0';
  clone->length=num_of_exprs_in_str(clone_str);
  if (clone->length< minlen)
    {
      clone->ranges=NULL;
      return;
    }
  else
    while(r!=NULL)
      {
	strid = r->string_index-1;
	start_pos = r->start_pos;
	strcpy(str, seq_array[strid]);
	strncpy(prev_str, str, start_pos-1);
	prev_str[start_pos-1]='\0';
	num_of_prev_exprs=num_of_exprs_in_str(prev_str);
	if ((start_pos!=1) && (str[start_pos-2]!=',') && (str[start_pos-1]!=','))
	  r-> start_pos=num_of_prev_exprs;
	else 
	  r->start_pos=num_of_prev_exprs+1;
	r = r->next;
      }
  return;
}

int over_lapped_range(int strid, int start_pos, int length, struct RANGE* ranges) 
{
  int found=0;
  struct RANGE* cur_range;
  cur_range=ranges;
  while(cur_range!=NULL && found==0)
    {
      if (cur_range->string_index==strid)
	if ((start_pos >= cur_range->start_pos) &&
	    (start_pos <cur_range->start_pos+length-1))
	  found=1;
      cur_range=cur_range->next;
    }
  return found;
}


struct clone* add_to_clone_list(struct clone* clone_list_head, struct clone* clone, 
				char** seq_array, int minlen, int minfreq)
{
  struct clone *temp, *temp1,*cur;
  struct RANGE *ranges, *cur_range, *range;
  
  int first=1;
  int inserted=0;
  int result;
  int i=0;
  
  temp =(CLONE*) malloc(sizeof(CLONE));
  temp->length=clone->length;
  temp->string=(char*) malloc(sizeof(char)*(clone->length+1));
  strcpy(temp->string, clone->string);
  cur_range=NULL;
  ranges=clone->ranges;
  while (ranges!=NULL) 
    {
      if (over_lapped_range(ranges->string_index, ranges->start_pos, clone->length, cur_range)==1)
	ranges=ranges->next;
      else
	{ i++;
	  range = (RANGE*) malloc(sizeof(RANGE)); 
	  range ->string_index=ranges->string_index;
	  range->start_pos=ranges->start_pos;
	  range->next=cur_range;
	  cur_range=range;
	  ranges=ranges->next;
	}
    }
  temp->freq=i;
  temp->ranges=cur_range;
  temp->next=NULL;
  temp->prev=NULL;
  map_a_clone(temp, seq_array, minlen, minfreq);
  if ((temp->length< minlen) || (temp->freq <-minfreq))
    { free_a_clone_node(temp);
      return clone_list_head;
    }
  if (clone_list_head==NULL)
    {
      inserted=1;
      clone_list_head=temp;
    }
  else
    {
      cur=clone_list_head;
      while(cur!=NULL)
      {
	result=sub_clone(temp, cur);
	//printf("Results: %d \n", result);
	if (result==0) /* clone is not a sub-clone of cur; neither the other way*/
	  {
	    if (cur-> next!=NULL)
	      cur=cur->next;
	    else
	      {
		if (inserted==0)
		  {
		    cur->next=temp;
		    temp->prev=cur;
		    cur=NULL;
		    inserted=1;
		  }
		else
		  cur=cur->next;
	      }
	  }
	else 
	  { if (result==1) /* temp is a sub-clone of cur; so do nothing */
	      	cur=NULL;
	    else    /* cur is a sun-clone of temp; do replace*/
	      {
		/* printf("result: %d", result); */
		/* printf("first: %d", first);  */
		if (first==1)
		  { temp->next=cur->next;
		    temp->prev=cur->prev;
		    if (cur->prev != NULL)
		      cur->prev->next=temp;
		    if (cur->next !=NULL) 
		      cur->next->prev=temp;
		    first=0;
		    inserted=1;
		    if (cur->prev==NULL)
		      clone_list_head=temp;
		    cur->next=NULL;
		    cur->prev=NULL;
		    free_a_clone_node(cur);
		    cur=temp->next;
		  }
		else
		  {
		    if (cur->next==NULL)
		      {
			cur->prev->next=NULL;
			cur->prev=NULL;
			free_a_clone_node(cur);
			cur=NULL;
		      }
		    else
		      {
			temp1=cur->next;
			cur->prev->next=cur->next;
			cur->next->prev=cur->prev;
			cur->next=NULL;
			cur->prev=NULL;
			free_a_clone_node(cur);
			cur=temp1;
		      }
		  }
	      }
	  }
      }
    }
   if (inserted==0)  
     free_a_clone_node(temp); 
  /* rintf("Inserted: %d\n", inserted); */
   return clone_list_head;
}
    



void collect_clones(struct stree *stree, int minlen, int minclones, char* filename, char** seq_array)
{

  enum { START, FIRST, MIDDLE, DONE, DONELEAF } state;
  int i, num, childnum;
  struct node *root, *child, *temp, *node;
  struct leaf_node *leaf;
  struct internal_node *int_node;
  struct int_leaf *int_leaf;
  char str_acc[4096]="\0";
  CLONE *clone, *collected_clones=NULL;
  int freq;
  RANGE *range;
  
  /* char clone_str[4096]="\0"; */
  int edge_len;
  
  /*
   * Use a non-recursive traversal where the `isaleaf' field of each node
   * is used as the value remembering the child currently being
   * traversed.
   */
  root =stree_get_root(stree);
  node=root;
  /* printf("root->str_acc: %s", root->str_acc); */
  state = START;
  while (1) {
    /* printf(" loop again\n"); */
    /*
     * The first time we get to a node.
     */
    if (state == START) {
      num = kTstree_get_num_children(stree, node);
       if (num > 0 && ((struct internal_node*)node)->leaves==NULL)
        { 
	  temp = node_get_parent(node);
	  if (temp ==NULL)
	    { 
	      node->str_acc=(char*) malloc(sizeof(char)*(node->edgelen+1));
	      strcpy(node->str_acc, "\0");
	      if (node->edgestr!=NULL)
		{ 
		  /* printf("node->str_acc: %s\n", node->str_acc); */
		  strncat(node->str_acc, node->edgestr, node->edgelen);
		}
	    }
	  else
	    {
	      node->str_acc=(char*) malloc(sizeof(char)*(strlen(temp->str_acc)+node->edgelen+1));
	      strcpy(node->str_acc, "\0");
	      strcpy(node->str_acc, temp->str_acc);
	      (node->str_acc)[strlen(temp->str_acc)]='\0';
	      /* printf("node->str_acc: %s\n", node->str_acc); */
	      strncat(node->str_acc, node->edgestr, node->edgelen);
	    }
	  strcpy(str_acc, node->str_acc);
	  str_acc[strlen(node->str_acc)]='\0';
	  state = FIRST;
	}
      else 
        { state = DONELEAF;
	  /* printf("STATE is DONELEAF\n"); */
	  if (node_isaleaf(node))
	    { leaf = (struct leaf_node*) node;
	      clone = (CLONE*) malloc(sizeof(CLONE));
	      edge_len=node_get_edgelen(node);
	      node->str_acc=(char*) malloc(sizeof(char)*(strlen(str_acc)+edge_len+1));
	      strcpy(node->str_acc, str_acc);
	      /* printf("str_acc: %s\n", str_acc); */
	      /* printf("node_edge_str: %s \n", node_get_edgestr(node)); */
	      /* printf("edge_len: %d\n", edge_len); */
	      strncat(node->str_acc, node_get_edgestr(node), edge_len);
	      /* printf("node->str_acc: %s\n", node->str_acc); */
	      clone->length = strlen(node->str_acc);
	      clone->freq = 1;
	      clone->string=(char *) malloc(sizeof(char)*((clone->length+1)));
	      strcpy(clone->string,node->str_acc);
	      range = (RANGE*) malloc(sizeof(RANGE));
	      range->string_index = leaf->strid+1;
	      range->start_pos = leaf-> pos+1;
	      range->next=NULL;
	      clone->ranges=range;
	      clone->next=NULL;
	      clone->prev=NULL;
	      (leaf->n).clone= clone;
	    }
	  else
	    { /* printf("This is a false leaf node\n");  */
	      int_node=(struct internal_node*) node;
	      clone =(CLONE *) malloc(sizeof(CLONE));
	      edge_len=node_get_edgelen(node);
	      node->str_acc=(char*) malloc(sizeof(char)*(strlen(str_acc)+node->edgelen+1));
	      strcpy(node->str_acc, str_acc);
	      strncat(node->str_acc, node_get_edgestr(node), edge_len);
	      clone->length = strlen(node->str_acc);
	      clone->string=(char *) malloc(sizeof(char)*(1+strlen(node->str_acc)));
	      strcpy(clone->string,node->str_acc);
	      int_leaf = ((struct internal_node*)node)->leaves;
	      freq=0;
	      RANGE *last=NULL;
	      for (;int_leaf != NULL;int_leaf=int_leaf->next)
		{
		  RANGE* temp_range = (RANGE*) malloc(sizeof(RANGE));
		  temp_range->string_index = int_leaf->strid+1;  
		  temp_range->start_pos = int_leaf-> pos+1; 
		  temp_range->next=last; 
		  last=temp_range; 
		  freq++;
		}
	      clone -> freq = freq;
	      clone->ranges=last;
	      clone -> next =NULL;
	      clone -> prev = NULL;
	      (int_node->n).clone=clone;
	      if (strcmp(node_get_edgestr(node), "$") && freq>=minclones &&
		  num_of_exprs_in_str(clone->string)>=minlen)
		 collected_clones=add_to_clone_list(collected_clones, clone, seq_array, minlen, minclones); 
	    }
	}
    }
    /*
     * Start or continue recursing on the children of the node.
     */
    if (state == FIRST || state == MIDDLE) {
      /*
       * Look for the next child to traverse down to.
       */
      if (state == FIRST)
	{ childnum = 0;
	}
      else
        { childnum = node->isaleaf;
	}
      /* TODO: Make this more efficient, there is no need to traverse this all time */
      for (child = ((struct internal_node*)node)->first,i=0;child != NULL && i<childnum;child = child->next,i++);

      if (child == NULL)
        state = DONE;
      else {
        node->isaleaf = i + 1;
        node = child;
        state = START;
      }
    }

    /*
     * Last time we get to a node, do the post-processing and move back up,
     * unless we're at the root of the traversal, in which case we stop.
     */

    if (state == DONE  || state == DONELEAF) {
      if (state == DONE && node!=root)
        {  node->isaleaf = 0; 
	   clone =  (CLONE *) malloc(sizeof(CLONE));
	   clone->string=(char *) malloc(sizeof(char)*(strlen(node->str_acc)+1));
	   strcpy(clone->string, node->str_acc); 
	   clone->length=strlen(clone->string); 
	   freq=0; 
	   RANGE *last=NULL, *child_range=NULL, *temp_range=NULL; 
	   for (temp =((struct internal_node*)node)->first; temp!=NULL; temp=temp->next) 
	    {
	      freq=freq+(temp->clone)->freq;
	      child_range=(temp->clone)->ranges;
	      for(; child_range!=NULL; child_range=child_range->next)
	  	{ temp_range = (RANGE*) malloc(sizeof(RANGE));
	  	  temp_range->string_index = child_range->string_index;
	  	  temp_range->start_pos = child_range-> start_pos;
	  	  temp_range->next=last;
	  	  last=temp_range;
	  	}
	    }
	  clone-> freq=freq; 
	  clone->ranges=last;
	  clone -> next =NULL; 
	  clone -> prev=NULL;
	  node->clone=clone; 
	  if (strcmp(node_get_edgestr(node), "$") && freq>=minclones
	      && num_of_exprs_in_str(clone->string)>=minlen)
	    collected_clones=add_to_clone_list(collected_clones, clone, seq_array, minlen, minclones);
	    //free_child_clones((struct internal_node *)node);
	}
      if (node == root)
        { //free_child_clones((struct internal_node *)node);
	  break;
	}
      node = node_get_parent(node);
      state = MIDDLE;
    }
  }
  /* print_clones(collected_clones);  */
  print_clones_to_file(filename, collected_clones); 
  free_clone_nodes(collected_clones); 
}



