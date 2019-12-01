PROGS = shell

include Makefile.include

CC += -fsanitize=address
CPPFLAGS += $(shell pkg-config --print-errors --cflags libedit)
LDFLAGS += $(shell pkg-config --print-errors --libs libedit)

shell: shell.o command.o helpers.o jobs.o

# vim: ts=8 sw=8 noet
