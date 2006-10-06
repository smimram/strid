SOURCES=wire.ml lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=menhir

all: nc

test: all
	./strid test.strid
	latex out.tex
	dvips out.dvi
	cat out.tex
	gv out.ps

include OCamlMakefile
