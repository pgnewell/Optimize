LIB = matrix.ml reader.ml simplex.ml
EXE = optimize.ml

all: lib exe

lib: $(LIB)
	ocamlc -o lib.cma -a Helpers.ml matrix.ml instrument.ml reader.ml 
	ocamlc -c Helpers.ml
	ocamlc -c matrix.ml
	ocamlc -c reader.ml
	ocamlc -c simplex.ml
	ocamlc -c instrument.ml

exe: $(EXE)
	ocamlc -o optimize str.cma Helpers.ml matrix.ml simplex.ml instrument.ml reader.ml optimize.ml
	ocamlc -o simplex str.cma Helpers.ml matrix.ml simplex.ml instrument.ml

#	ocamlc str.cma matrix.cmo simplex.cmo instrument.cmo reader.cmo -o optimize optimize.ml
#	ocamlc str.cma matrix.cmo instrument.cmo -o simplex simplex.ml

clean:
	rm -f *.cmi *.cmo *.cma optimize
