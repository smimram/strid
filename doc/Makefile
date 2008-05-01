STRID=../src/strid
STRIDFILES=$(wildcard *.strid)
STRIDPDF=$(STRIDFILES:.strid=.pdf)

all: stridman.pdf

stridman.pdf: $(STRIDPDF) bicat_nat_transfo_l.tex bicat_nat_transfo_r.tex

clean:
	rm -f *.log *.aux
	rm -f $(STRIDPDF)

distclean: clean
	rm -f stridman.pdf

bicat_nat_transfo_l.tex: bicat_nat_transfo_l.strid
	$(STRID) $<

bicat_nat_transfo_r.tex: bicat_nat_transfo_r.strid
	$(STRID) $<

%.pdf: %.strid strid.conf
	$(STRID) --pdf $<

%.pdf: %.tex
	pdflatex $<
