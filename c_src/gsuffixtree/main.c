
#include <ei.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h> /*  _O_BINARY; this is needed for Windows*/
#include "gsuffix_tree.h"

#define BUF_SIZE 250

typedef char mybyte;


int read_cmd(mybyte *buf, int *size);
int write_cmd(ei_x_buff* x);
int read_exact(mybyte *buf, int len);
int write_exact(mybyte *buf, int len);


struct count { int n; };
int callback(int index, int pos, void *userdata)
{
  struct count *c = (struct count *)userdata;
  c->n++;
  printf("%d) seq.: %d, pos.: %d.\n",c->n, index+1,pos);
  return 1;
}

void free_seq_array(char **array, int size)
{
  int i;
  for(i=0; i<size; i++)
    free(array[i]);
   free(array);
}
  

int clone_detection_by_suffix_tree(char *filename, long minlen, long minclones, long overlap_allowed)
{
  FILE* file = 0;
  char* *seq_array;
  char str[2048]="\0";
  int array_index=0;
  int num_of_strings;
  struct stree *stree;
  file = fopen((const char*)filename,"r");
  /*Check for validity of the file.*/
  if(file == 0)
    {
      printf("Failed to open file: %s.\n", filename);
      return 0;
    }
  fscanf(file, "%d", &num_of_strings);
  /* printf("Num of strings: %d", num_of_strings); */
  seq_array = (char * *) malloc((num_of_strings+1)*sizeof(char *));
  array_index=0;
  while(fscanf(file, "%s", str)==1)
    { seq_array[array_index]=malloc((strlen(str)+1)*sizeof(char));
      strcpy(seq_array[array_index], str);
      array_index++;
    }
  fclose(file);
  if ((stree=stree_create_from_char(seq_array, array_index)))
    { collect_clones(stree, minlen, minclones, filename, seq_array);
      free_seq_array(seq_array, num_of_strings);
      stree_delete_tree(stree);
      return 1;
    }
  return 0;
}


int main()
{
    char  *filename;
    
    mybyte*     buf;
    int       size = 500;
    char      command[MAXATOMLEN];
    int       index, version, arity;
    long      a, b, d, o;
    ei_x_buff result;
	
#ifdef _WIN32
	/* Attention Windows programmers: you need to explicitly set
	 * mode of stdin/stdout to binary or else the port program won't work
	 */
	setmode(fileno(stdout), O_BINARY);
	setmode(fileno(stdin), O_BINARY);
#endif
	if ((buf = (mybyte *) malloc(size)) == NULL)
	    return -1;
	if ((filename=(char *) malloc(size)) ==NULL)
	    return -1;
	while (read_cmd(buf, &size) > 0) {
	    /* Reset the index, so that ei functions can decode terms from the
	     * beginning of the buffer */
	    index = 0;
	    
	    /* Ensure that we are receiving the binary term by reading and
	     * stripping the version byte */
	    if (ei_decode_version(buf, &index, &version)) return 1;
    
	    /* Our marshalling spec is that we are expecting a tuple {Command, Arg1, Arg2, Arg3, Arg4} */
	    if (ei_decode_tuple_header(buf, &index, &arity)) return 2;
	     
	    if (arity != 5) return 3;
    
	    if (ei_decode_atom(buf, &index, command)) return 4;
    
	    /* Prepare the output buffer that will hold {ok, Result} or {error, Reason} */
	    if (ei_x_new_with_version(&result) || ei_x_encode_tuple_header(&result, 2)) return 5;
	    
	    if (!strcmp("get", command)){
		if (ei_decode_long(buf, &index, &a)) return 6;
		if (ei_decode_long(buf, &index, &b)) return 7;
		if (ei_decode_long(buf, &index, &o)) return 8;
		if (ei_decode_string(buf, &index, filename)) return 9;
		clone_detection_by_suffix_tree(filename, a, b, o);
		d = a+b+o;
		if (ei_x_encode_atom(&result, "ok") || ei_x_encode_long(&result, d)) return 10;
		
	    }  else {
	      if (ei_x_encode_atom(&result, "error") || ei_x_encode_atom(&result, "unsupported_command"))
		return 99;
	    }
	    
	    write_cmd(&result);
	    
	    ei_x_free(&result);
	}
	return 0;
}



/*-----------------------------------------------------------------
 * Data marshalling functions
 *----------------------------------------------------------------*/
int read_cmd(mybyte *buf, int *size)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];

  if (len > *size) {
    buf = (mybyte *) realloc(buf, len);
    if (buf == NULL)
      return -1;
    *size = len;
  }
  return read_exact(buf, len);
}

int write_cmd(ei_x_buff *buff)
{
  mybyte li;

  li = (buff->index >> 8) & 0xff; 
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);

  return write_exact(buff->buff, buff->index);
}

int read_exact(mybyte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return i;
    got += i;
  } while (got<len);

  return len;
}

int write_exact(mybyte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}


