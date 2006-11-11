PROG = strid
DISTFILES = COPYING README Makefile src/Makefile src/OCamlMakefile src/*.ml src/*.mll src/*.mly
VERSION = 0.1.0

all clean:
	make -C src $@

dist:
	mkdir $(PROG)-$(VERSION)
	cp -r --parents $(DISTFILES) $(PROG)-$(VERSION)
	tar zcvf $(PROG)-$(VERSION).tar.gz $(PROG)-$(VERSION)
	rm -rf $(PROG)-$(VERSION)

.PHONY: dist
