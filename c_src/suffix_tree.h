/******************************************************************************
Suffix Tree Version 2.1
by:         Dotan Tsadok.
Instructor: Mr. Shlomo Yona.
University of Haifa, Israel.
December 2002.

Current maintainer:
	Shlomo Yona	<shlomo@cs.haifa.ac.il>

DESCRIPTION OF THIS FILE:
This is the decleration file suffix_tree.h and it contains declarations of the
interface functions for constructing, searching and deleting a suffix tree, and
to data structures for describing the tree.

COPYRIGHT
Copyright 2002-2003 Shlomo Yona

LICENSE
This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.
*******************************************************************************/

/* A type definition for a 32 bits variable - a double word. */
#define     WORD      unsigned int   

/* Error return value for some functions. Initialized  in ST_CreateTree. */
WORD    ST_ERROR;

/******************************************************************************/
/*                           DATA STRUCTURES                                  */
/******************************************************************************/

/* This structure defines a range */
typedef struct RANGE
{
    /* Start index of the incoming edge */
   WORD                 start;
   /* End index of the incoming edge */
   WORD                 end;

   struct RANGE*                  next;
 }RANGE;

/* This structure describes a node and its incoming edge */
typedef struct SUFFIXTREENODE
{
    /* A linked list of sons of that node */
    struct SUFFIXTREENODE*   sons;
    /* A linked list of right siblings of that node */
    struct SUFFIXTREENODE*   right_sibling;
    /* A linked list of left siblings of that node */
    struct SUFFIXTREENODE*   left_sibling;
    /* A pointer to that node's father */
    struct SUFFIXTREENODE*   father;
    /* A pointer to the node that represents the largest 
      suffix of the current node */
    struct SUFFIXTREENODE*   suffix_link;
    /* Index of the start position of the node's path */
    WORD                 path_position;
    /* Start index of the incoming edge */
    WORD                 edge_label_start;
    /* End index of the incoming edge */
    WORD                 edge_label_end;
    /* The number of time the substring is duplicated */
    int                      frequency;
    struct RANGE*            ranges;
} NODE;

/* This structure describes a suffix tree */
typedef struct SUFFIXTREE
{
   /* The virtual end of all leaves */
   WORD                 e;
   /* The one and only real source string of the tree. All edge-labels
      contain only indices to this string and do not contain the characters
      themselves */
   char*           tree_string;
   /* The length of the source string */
   WORD                 length;
   /* The node that is the head of all others. It has no siblings nor a
      father */
   NODE*                    root;
} SUFFIX_TREE;



/******************************************************************************/
/*                         INTERFACE FUNCTIONS                                */
/******************************************************************************/
/* 
   ST_CreateTree :
   Allocates memory for the tree and starts Ukkonen's construction algorithm by
   calling SPA n times, where n is the length of the source string.

   Input : The source string and its length. The string is a sequence of
           characters (maximum of 256 different symbols) and not
           null-terminated. The only symbol that must not appear in the string
           is $ (the dollar sign). It is used as a unique symbol by the
           algorithm and is appended automatically at the end of the string (by
           the program, not by the user!). The meaning of the $ sign is
           connected to the implicit/explicit suffix tree transformation,
           detailed in Ukkonen's algorithm.

   Output: A pointer to the newly created tree. Keep this pointer in order to
           perform operations like search and delete on that tree. Obviously,
           no de-allocating of the tree space could be done if this pointer is
           lost, as the tree is allocated dynamically on the heap.
*/

SUFFIX_TREE* ST_CreateTree(unsigned char*   str, WORD length);

/******************************************************************************/
/*
   ST_FindSubstring :
   Traces for a string in the tree. This function is used for substring search
   after tree construction is done. It simply traverses down the tree starting
   from the root until either the searched string is fully found ot one
   non-matching character is found. In this function skipping is not enabled
   because we don't know wether the string is in the tree or not (see function
   trace_string above).

   Input : The tree, the string W, and the length of W.

   Output: If the substring is found - returns the index of the starting
           position of the substring in the tree source string. If the substring
           is not found - returns ST_ERROR.
*/

WORD ST_FindSubstring(SUFFIX_TREE*      tree,   /* The suffix array */
                          char*    W,      /* The substring to find */
                          WORD          P);     /* The length of W */

/******************************************************************************/
/*
   ST_PrintTree :
   This function prints the tree. It simply starts the recoursive function
   ST_PrintNode from the root

   Input : The tree to be printed.
  
   Output: A print out of the tree to the screen.
*/

void ST_PrintTree(SUFFIX_TREE* tree);

/******************************************************************************/
/*
   ST_DeleteTree
   Deletes a whole suffix tree by starting a recoursive call to ST_DeleteSubTree
   from the root. After all of the nodes have been deleted, the function deletes
   the structure that represents the tree.

   Input : The tree to be deleted.

   Output: None.
*/

void ST_DeleteTree(SUFFIX_TREE* tree);

/******************************************************************************/
/*
   ST_SelfTest
   Self test of the tree - search for all substrings of the main string. See
   testing paragraph in the readme.txt file.

   Input : The tree to test.

   Output: 1 for success and 0 for failure. Prints a result message to the
           screen.
*/
WORD ST_SelfTest(SUFFIX_TREE* tree);


/******************************************************************************/
void ST_CollectClones(SUFFIX_TREE* tree, int len, int f, char *filename);

SUFFIX_TREE* ST_Add_Freq_And_Range(SUFFIX_TREE* tree);

SUFFIX_TREE* ST_ExtendRanges(SUFFIX_TREE* tree);

void clone_detection_by_suffix_tree(char *filename, long minlen, long minclones);
