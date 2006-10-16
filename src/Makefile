SOURCES=common.ml spline.ml wire.ml lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=menhir
LIBS=str bigarray gsl
INCDIRS=+gsl

all: bc

test: all
	./strid --full-tex test.strid
	latex test.tex
	dvips test.dvi
	cat test.tex

%.tex: %.strid
	./strid --full-tex $<

%.ps: %.tex
	latex $<
	dvips `basename $< .tex`.dvi

include OCamlMakefile
