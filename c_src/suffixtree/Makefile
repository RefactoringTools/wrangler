 

ERL_CFLAGS  = -Ic:/erl5.8/lib/erl_interface-3.7/include -Ic:/erl5.8/usr/include
ERL_LIB     = -Lc:/erl5.8/lib/erl_interface-3.7/lib -lerl_interface -lei
CFLAGS      = -g -O2 -Wall -m32
LDFLAGS     =  -m32

#CFLAGS=-Wall -I/usr/local/include ${ERL_CFLAGS}
ERL_LDFLAGS= ${LDFLAGS} ${ERL_LIB}


suffixtree:	main.o suffix_tree.o
	gcc  ${ERL_LDFLAGS} main.o suffix_tree.o -lerl_interface -lei -lpthread -o suffixtree 

suffix_tree.o:	suffix_tree.c suffix_tree.h
	gcc ${CFLAGS} ${ERL_CFLAGS} -o suffix_tree.o -c suffix_tree.c

main.o:	suffix_tree.h 
	gcc ${CFLAGS} ${ERL_CFLAGS} -o main.o -c main.c 

clean: 
	-rm -f *.o 
	-rm -f suffixtree* 

