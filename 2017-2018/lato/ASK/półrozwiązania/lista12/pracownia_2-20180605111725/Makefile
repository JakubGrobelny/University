UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
CC = gcc -g
endif
ifeq ($(UNAME), Darwin)
CC = clang -g
endif

CFLAGS	= -std=gnu11 -march=native -O2 -Wall -Wextra

BINS = bsearch cache matmult transpose randwalk
SRCS = $(wildcard *.c *.h *.md *.gp *.dat Makefile)

all: $(BINS) # raport.html

bsearch: bsearch.o common.o
bsearch.o: bsearch.c common.h
cache: cache.o common.o
cache.o: cache.c common.h
matmult: matmult.o common.o
matmult.o: matmult.c common.h
randwalk: randwalk.o common.o
randwalk.o: randwalk.c common.h
transpose: transpose.o common.o
transpose.o: transpose.c common.h
common.o: common.c common.h

# requires "markdown" and "gnuplot" packages to be installed
raport.html: raport.md figure.png

%.html: %.md
	markdown $< > $@

%.png: %.gp %.dat
	gnuplot $< > $@

%.eps: %.gp %.dat
	gnuplot $< > $@

clean:
	@rm -vf *.o *.html *.png *~ $(BINS)

dist:
	mkdir -p stripped 
	cp -a $(SRCS) stripped/
	cd stripped && \
	  for f in *.c; do \
	    sed -i -e '/^#if.*SOLUTION/,/^#endif.*SOLUTION/d' $$f; \
	  done
	cd stripped && tar cvzf ../../pracownia_2-$$(date +'%Y%m%d%H%M%S').tgz *
	rm -rf stripped

# vim: ts=8 sw=8
