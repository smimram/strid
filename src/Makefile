SOURCES=lang.ml parser.mly lexer.mll strid.ml
RESULT=strid
OCAMLYACC=menhir

all: nc

test: all
	./strid test.strid

include OCamlMakefile
