CC = gcc -g
CFLAGS = -Og -Wall
CPPFLAGS = -Iinclude -DLINUX -D_GNU_SOURCE 
LDFLAGS = -Llibcsapp -lcsapp

LIBCSAPP = libcsapp/libcsapp.a 
LIBCSAPP_SRC = $(wildcard libcsapp/*.c)
LIBCSAPP_OBJ = $(patsubst %.c,%.o,$(LIBCSAPP_SRC))

ARCHIVE = so$(shell date +'%y')_$(shell basename $(PWD))
FILES = *.c libcsapp include test.txt Makefile 
PROGS =	fileref het

all: $(PROGS)

%: %.c $(LIBCSAPP)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS) $(LDLIBS)

$(LIBCSAPP): $(LIBCSAPP_OBJ)
	$(AR) rc $@ $^

clean:
	rm -f $(PROGS) *.o $(shell find . -iname '*~')
	rm -f $(LIBCSAPP) $(LIBCSAPP_OBJ)
	rm -f $(ARCHIVE).tar.gz

archive: clean
	mkdir -p $(ARCHIVE)
	cp -ar $(FILES) $(ARCHIVE)
	for f in $$(find $(ARCHIVE) -iname '*.[ch]'); do \
	  sed -i -e '/^#if.*STUDENT/,/^#endif.*STUDENT/d' $$f; \
	done
	tar cvf $(ARCHIVE).tar.gz $(ARCHIVE)
	rm -rf $(ARCHIVE)

.PHONY: all clean archive

# vim: ts=8 sw=8 noet
