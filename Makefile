PROG = strid
DISTFILES = COPYING README Makefile strid.1 src/Makefile src/OCamlMakefile src/*.ml src/*.mll src/*.mly
VERSION = 0.1.0

all clean:
	make -C src $@

install: all
	mkdir -p $(DESTDIR)/usr/bin
	cp src/strid $(DESTDIR)/usr/bin
	mkdir -p $(DESTDIR)/usr/share/man/man1
	cp strid.1 $(DESTDIR)/usr/share/man/man1

dist:
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

.PHONY: dist
