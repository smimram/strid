SOURCES=strid_glade.ml main.ml
RESULT=gstrid
INCDIRS=../src +lablgtk2
THREADS=yes

OCAMLBLDFLAGS = lablgtk.cma gtkInit.cmo gtkThread.cmo lablglade.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx gtkThread.cmx lablglade.cmxa

test: all
	./gstrid

all: strid_glade.ml nc

strid_glade.ml: gtkstrid.glade
	lablgladecc2 $< > $@
	ocamlc -I +lablgtk2 -i $@ > $@i

include OCamlMakefile
