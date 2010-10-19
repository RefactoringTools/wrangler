#ifndef SUFFIX__GMEM_H
#define SUFFIX__GMEM_H

/**************************************************/

void *gsuffix_malloc(int size);
void gsuffix_free(void *);
void *gsuffix_realloc(void *mem, int size);

/**************************************************/

unsigned int gsuffix_memallocated(void);
unsigned int gsuffix_maxmem(void);
void gsuffix_resetmaxmem(void);

/**************************************************/

struct item_pool;

struct item_pool *item_pool_create(int item_size);
void item_pool_delete(struct item_pool *ip);
void *item_pool_alloc(struct item_pool *pool);
void item_pool_free(struct item_pool *pool, void *item);

/**************************************************/

struct freeable_item_pool;

struct freeable_item_pool *freeable_item_pool_create(int freeable_item_size);
void freeable_item_pool_delete(struct freeable_item_pool *ip);
void *freeable_item_pool_alloc(struct freeable_item_pool *pool);
void freeable_item_pool_free(struct freeable_item_pool *pool, void *item);

#endif



#ifndef KTSTREE_H_
#define KTSTREE_H_

/* opaque type */
struct stree;


/* version using short (i.e. 16bit) integers to store string indices */
struct stree *shkTstree_create_from_char(char **strings, int nstrings, int query_length);

int gsuffix_exists(struct stree *tree, const char *pattern, int pattern_length);
int shstree_enum_k_mers(struct stree *tree, int k,
				       int (*callback)(char *kmer, int id, int pos, void *userdata),
				       void *userdata);
int shsuffixTree_lookup(struct stree *tree,
			const char *pattern,
			int pattern_length,
			int (*hitcallback)(int index,
					   int pos,
					   void *userdata),
			void *userdata);


/* version using long (i.e. 32bit) integers to store string indices. Note: Never mix up
 * the two types! */
struct stree *lokTstree_create_from_char(char **strings, int nstrings, int query_length);
void lokTstree_delete_tree(struct stree *tree);
int gsuffix_lookup(struct stree *tree,
				 const char *pattern,
				 int pattern_length,
				 int (*hitcallback)(int index,
						    int pos,
						    void *userdata),
				 void *userdata);
int gsuffix_exists(struct stree *tree, const char *pattern, int pattern_length);
int lostree_enum_k_mers(struct stree *tree, int k,
				       int (*callback)(char *kmer, int id, int pos, void *userdata),
				       void *userdata);

/****  version using short (i.e. 16bit) integers to store string indices ****/


/*** version using long (i.e. 32bit) integers to store string indices.
     Note: Never mix up  the two types! ****/
struct stree *lostree_create_from_char (char **strings, int nstrings);
void lostree_delete_tree(struct stree *tree);
int losuffixTree_lookup(struct stree *tree,
			const char *pattern,
			int pattern_length,
			int (*hitcallback)(int index,
					   int pos,
					   void *userdata),
			void *userdata);


struct stree *stree_create_from_char (char **strings, int nstrings);

/*********************************************************************
 *   gsuffix is an opaque type, that is, client code should   *
 *   know or care what exactly is inside it.                         *
 *********************************************************************/

/**
 * @brief An opaque data type representing an index.
 * 
 * You shouldn't care about its actual contents. Create it via gsuffix_create().
 * 
 * @see gsuffix_create()
 */
struct gsuffix { };

/**
 * @}
 */


#define GSUFFIX_LOTRUNCSUFFIXTREE GSUFFIX_TRUNCSUFFIXTREE

/**
 * @deprecated
 */
#define GSUFFIX_LOSUFFIXTREE GSUFFIX_SUFFIXTREE


enum gsuffix_type
{
    GSUFFIX_SUFFIXARRAY,      //!< A generalized suffix array.
    GSUFFIX_TRUNCSUFFIXTREE,//!< A truncated suffix tree.
//    GSUFFIX_SHTRUNCSUFFIXTREE,//!< GSUFFIX_SHTRUNCSUFFIXTREE
    GSUFFIX_HASH,             //!< A basic hash index.
    GSUFFIX_SUFFIXTREE,     //!< A suffix tree.
//    GSUFFIX_SHSUFFIXTREE,     //!< GSUFFIX_SHSUFFIXTREE
    GSUFFIX_DIRECT            //!< A direct approach (similar to hash)
};

struct gsuffix *gsuffix_create(char **strings, int nstrings, enum gsuffix_type type, int query_length, int options);
void gsuffix_delete(struct  gsuffix *gs);
void kTstree_traverse(struct stree *tree, int (*preorder_fn)(),int (*postorder_fn)());
void stree_print(struct stree *tree);
/*********************************************************************
 *  There are three kinds of lookup function.                        *
 *  1) gsuffix_lookup is used to look up an individual query sequence*
 *  in one of the datastructures. gs is the datastructure created as *
 *  above, p is the pattern to be sought, m is the length of p , hit *
 *  callback is a callback function that expects to receive the      *
 *  arguments index for the index of the sequence in which the       *
 *  pattern is found, and pos for the position of the pattern in the *
 *  sequence, and userdata, a user-defined structure in which to     *
 *  store the results. Note that this callback function will be      *
 * called once for each match for p in all of the input sequences.   *
 *  2) gsuffix_lookup_exists looks to see if the sequence 'p' exists *
 *     in the input data represented by the suffix datastructure.    *
 *     After the first match, the function returns. The callback     *
 *     function does not get information about the sequence and      *
 *     position where 'p' was found.                                 *
 *  3) gsuffix_enum_kmers. Finds each ocurrence of every substring   *
 *       of length k in the input sequences. The callback function   *
 *      is called once for each hit  and gets information about the  *
 * sequence and position within the sequence of the hit.             *
 *********************************************************************/
int gsuffix_lookup_exists(struct stree *stree, const char *p, int m);
int gsuffix_enum_k_mers(struct gsuffix *gs, int k,
			int (*callback)(char *kmer, int id, int pos, void *userdata),
			void *userdata);


void *gsuffix_malloc(int size);
void gsuffix_free(void *);
void *gsuffix_realloc(void *mem, int size);

void collect_clones(struct stree *stree, int minlen, int minclones, char* filename, char** seq_array);

void stree_delete_tree(struct stree *tree);
/**************************************************/

unsigned int gsuffix_memallocated(void);
unsigned int gsuffix_maxmem(void);
void gsuffix_resetmaxmem(void);

/**************************************************/

struct item_pool;

struct item_pool *item_pool_create(int item_size);
void item_pool_delete(struct item_pool *ip);
void *item_pool_alloc(struct item_pool *pool);
void item_pool_free(struct item_pool *pool, void *item);

/**************************************************/

struct freeable_item_pool;

struct freeable_item_pool *freeable_item_pool_create(int freeable_item_size);
void freeable_item_pool_delete(struct freeable_item_pool *ip);
void *freeable_item_pool_alloc(struct freeable_item_pool *pool);
void freeable_item_pool_free(struct freeable_item_pool *pool, void *item);

#endif
