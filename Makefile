PROG = strid
DISTFILES = COPYING CHANGES README Makefile strid.1 src/Makefile src/OCamlMakefile src/*.ml src/*.mli src/*.mll src/*.mly doc/stridman.tex doc/stridman.pdf doc/Makefile doc/*.strid doc/strid.conf
VERSION = 0.1.0

BINDIR=$(DESTDIR)/usr/local/bin
MANDIR=$(DESTDIR)/usr/local/share/man/man1

all:
	make -C src $@

clean:
	make -C src $@
	make -C doc $@

distclean:
	make -C src $@
	make -C doc $@

install: all
	mkdir -p $(BINDIR)
	cp src/strid $(BINDIR)
	mkdir -p $(MANDIR)
	cp strid.1 $(MANDIR)

doc: all
	make -C doc

dist: doc
	mkdir $(PROG)-$(VERSION)
	cp -r --parents $(DISTFILES) $(PROG)-$(VERSION)
	tar zcvf $(PROG)-$(VERSION).tar.gz $(PROG)-$(VERSION)
	rm -rf $(PROG)-$(VERSION)

deb: dist
	rm -rf deb
	mkdir -p deb
	cp strid-$(VERSION).tar.gz deb/strid_$(VERSION).orig.tar.gz
	cd deb/; tar zxvf strid_$(VERSION).orig.tar.gz
	cp -r debian deb/strid-$(VERSION)
	rm -rf deb/strid-$(VERSION)/.svn
	cd deb/strid-$(VERSION); debuild

.PHONY: dist doc
