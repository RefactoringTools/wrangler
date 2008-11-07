ERL_LIB=c:/erl5.6.3/lib/erl_interface-3.5.7
CFLAGS=-Wall -I/usr/local/include -I${ERL_LIB}/include
LDFLAGS=-L. -L${ERL_LIB}/lib

DFLAGS = -ansi -pedantic -Wall -g

suffixtree:	main.o suffix_tree.o
	gcc  ${LDFLAGS} main.o suffix_tree.o -lerl_interface -lei -lpthread -o suffixtree 

suffix_tree.o:	suffix_tree.c suffix_tree.h
	gcc -g ${CFLAGS} -o suffix_tree.o -c suffix_tree.c

main.o:	suffix_tree.h 
	gcc -g ${CFLAGS} -o main.o -c main.c 

clean: 
	rm *.o 
	rm suffixtree.exe
