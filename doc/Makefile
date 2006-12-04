STRID=../src/strid
STRIDFILES=$(wildcard *.strid)
STRIDTEX=$(STRIDFILES:.strid=.tex)

all: $(STRIDTEX) stridman.pdf

clean:
	rm -f *.log *.aux
	rm -f $(STRIDTEX)

distclean:
	rm -f stridman.pdf

%.tex: %.strid
	$(STRID) -t tikz --scale 0.6 $<

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
