
include ../config/include.mk

## don't build this under win32 at all
ifdef WIN32

PRIV_FILES = 

else

PRIV_FILES=../priv/FILE_drv.so 

endif


CFLAGS += -I$(ERL_C_INCLUDE_DIR) -I../config -I.

#
# Targets
#

all: $(PRIV_FILES)

clean:
	-rm -f $(PRIV_FILES) FILE_drv.o 


install: 
	install -d $(ERLDIR)/lib/bfile
	cp -r `pwd` $(ERLDIR)/lib/bfile
	cp -r `pwd`/../ebin $(ERLDIR)/lib/bfile
	cp -r `pwd`/../priv $(ERLDIR)/lib/bfile


../priv/FILE_drv.so: FILE_drv.o 
	$(LD_SHARED) -o $@ FILE_drv.o $(LIBS)


FILE_drv.o: FILE_drv.c
	$(CC) -o $@ -c -fpic $(CFLAGS) -DDYNAMIC_DRIVER FILE_drv.c


