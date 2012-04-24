LIB = matrix.ml reader.ml simplex.ml
EXE = optimize.ml

all: lib exe

lib: $(LIB)
	ocamlc -o matrix.cma -a matrix.ml
	ocamlc -o reader.cma -a reader.ml
	ocamlc -o simplex.cma -a simplex.ml
	ocamlc -o instrument.cma -a instrument.ml

exe: $(EXE)
	ocamlc str.cma matrix.cma reader.cma simplex.cma instrument.cma -o optimize optimize.ml

clean:
	rm -f *.cmi *.cmo *.cma optimize
