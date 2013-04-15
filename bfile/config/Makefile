MK_INCLUDE=include.mk
CONFIG_H=config.h

all: config.status $(MK_INCLUDE) $(CONFIG_H)

config.status: configure
	./configure

$(CONFIG_H) $(MK_INCLUDE): config.status include.mk.in config.h.in
	./config.status

configure: configure.in
	autoheader
	autoconf

clean:
	-rm -f config.cache config.log config.status configure \
               $(MK_INCLUDE) $(CONFIG_H)
	-rm -rf autom4te.cache


