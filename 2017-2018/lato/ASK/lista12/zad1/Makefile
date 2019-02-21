CC	= gcc -g
CFLAGS	= -std=gnu11 -march=native -O2 -Wall

BINS = bsearch cache transpose randwalk

all: $(BINS)

bsearch: bsearch.o common.o
bsearch.o: bsearch.c common.h
cache: cache.o common.o
cache.o: cache.c common.h
randwalk: randwalk.o common.o
randwalk.o: randwalk.c common.h
transpose: transpose.o common.o
transpose.o: transpose.c common.h
common.o: common.c common.h

clean:
	@rm -vf *.o *~ $(BINS)

# vim: ts=8 sw=8
