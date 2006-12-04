SOURCES=conf.ml env.ml common.ml cubic_spline.ml spline.ml wire.ml lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=ocamlyacc
#INCDIRS=+gsl
LIBS=str #bigarray gsl

all: dc

test: all
	./strid --full-tex test.strid
	pdflatex test.tex
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
