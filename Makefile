CPPFLAGS = $(shell guile-config compile)
CFLAGS = -Wall -g -O0

snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)
SUFFIXES = .x

all: libguile-dynamic-reader.so

%.c.x: %.c
	guile-snarf-1.7 -o $@ $< $(snarfcppopts)

reader.o: reader.c reader.c.x
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

libguile-dynamic-reader.so: reader.o token-readers.o
	$(CC) -shared -o $@ $^


.PHONY: all
