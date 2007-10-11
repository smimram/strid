SOURCES=common.ml vect.ml conf.ml cubic_spline.ml spline.ml wire.mli wire.ml term.ml lang.mli lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=ocamlyacc
LIBS=str unix graphics

all: nc

test: all
	./strid --full-tex test.strid
	pdflatex test.tex > /dev/null
#	cat test.tex

tikz: all
	./strid --full-tex -t tikz test.strid
	latex test.tex
	dvips test.dvi
	cat test.tex

%.tex: %.strid
	./strid --full-tex $<

%.ps: %.tex
	latex $<
	dvips `basename $< .tex`.dvi

include OCamlMakefile
