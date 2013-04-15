

all: 
	(cd config;$(MAKE))
	(cd src;$(MAKE))
	-(cd c_src;$(MAKE) -k)
	$(MAKE) appfile

clean:
	(cd src;$(MAKE) clean)
	(cd c_src;$(MAKE) clean)
	(cd config; $(MAKE) clean)


install: all
	(cd c_src; $(MAKE) install)

conf_clean:
	(cd config; $(MAKE) clean)

appfile:
	(cd src;$(MAKE) ../ebin/bfile.app)

