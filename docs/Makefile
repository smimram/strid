SITE = ../_site

all:

website:
	$(MAKE) -C manual
	mkdir -p $(SITE)
	cp *.html *.css *.png $(SITE)
	cp manual/stridman.pdf $(SITE)
