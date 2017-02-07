.PHONY: programs all clean distclean
CFLAGS = --std=gnu11 -Wall -Wextra -Wpedantic
CC = gcc $(CFLAGS)
ASSEMBLE = ./assemble
SAMPLE_PROGS = sum.sextium halt.sextium gcd.sextium

sextium: sexproc.o sextium.c sexproc.h sextium.c
	$(CC) -o sextium sexproc.o sextium.c

sexproc.o: sexproc.c sexproc.h
	$(CC) -Wno-type-limits -c sexproc.c

$(SAMPLE_PROGS): %.sextium: %.sasm
	$(ASSEMBLE) $< $@ 2>/dev/null

programs: $(SAMPLE_PROGS)

all: sextium programs

clean:
	rm -f *.o

distclean: clean
	rm -f sextium *.sextium
