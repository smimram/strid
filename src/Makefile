SOURCES=common.ml spline.ml wire.ml lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=menhir
LIBS=str gsl
INCDIRS=+gsl

all: nc

test: all
	./strid --full-tex test.strid
	latex strid.tex
	dvips strid.dvi
	cat strid.tex
	#gv strid.ps

%.tex: %.strid
	./strid $< -o $@

%.ps: %.tex
	latex $<
	dvips `basename $< .tex`.dvi

include OCamlMakefile
