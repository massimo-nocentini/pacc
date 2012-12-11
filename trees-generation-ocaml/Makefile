test:
	ocamlfind ocamlc -o test.exe -package oUnit -linkpkg -g RandomTreesGenerator.ml test.ml
	./test.exe

utility:
	ocamlfind ocamlc -o treesUtility.ocaml.bytecode -package unix -linkpkg -g treesUtility.ml
	cp treesUtility.ocaml.bytecode ../random-trees-generation

clean:
	rm *.cmi
	rm *.cmo
	rm *.exe

# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
#SOURCES = random-trees-generator.ml

# the name of the resulting executable
#RESULT  = executable

# generate type information (.annot files)
#ANNOTATE = yes

# make target (see manual) : byte-code, debug-code, native-code, ...
#all: debug-code

#include OCamlMakefile