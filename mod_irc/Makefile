# XXX: these paths shouldn't be hardcoded
CC = gcc
CFLAGS = -O2 -Wall
CPPFLAGS = -I/usr/pkg/include -I/usr/local/lib/erlang/lib/erl_interface-3.5.5/include -I/usr/local/lib/erlang/usr/include -I/usr/local/ssl/lib -I/usr/pkg/include
LDFLAGS = -L/usr/pkg/lib -L/usr/local/lib/erlang/lib/erl_interface-3.5.5/lib -lerl_interface -lei -liconv
 
# Assume Linux-style dynamic library flags
DYNAMIC_LIB_CFLAGS = -fpic -shared
ifeq ($(shell uname),Darwin)
    DYNAMIC_LIB_CFLAGS = -fPIC -bundle -flat_namespace -undefined suppress
endif
ifeq ($(shell uname),SunOs)
    DYNAMIC_LIB_CFLAGS = -KPIC -G -z text
endif

ebin/iconv_erl.so:	src/iconv_erl.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $< -o $@ $(DYNAMIC_LIB_CFLAGS)

clean:
	rm -f src/iconv_erl.so
