SOURCES=common.ml spline.ml wire.ml lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=menhir
LIBS=str

all: nc

test: all
	./strid --full-tex test.strid
	latex out.tex
	dvips out.dvi
	cat out.tex
	gv out.ps

%.ps: %.tex
	latex $<
	dvips `basename $< .tex`.dvi

include OCamlMakefile
