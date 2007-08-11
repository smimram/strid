STRID=../src/strid
STRIDFILES=$(wildcard *.strid)
STRIDTEX=$(STRIDFILES:.strid=.tex)

all: $(STRIDTEX) stridman.pdf

stridman.pdf: $(STRIDTEX)

clean:
	rm -f *.log *.aux
	rm -f $(STRIDTEX)

distclean: clean
	rm -f stridman.pdf

%.tex: %.strid strid.conf
	$(STRID) $<

%.pdf: %.tex
	pdflatex $<
