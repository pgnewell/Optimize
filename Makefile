LIB = matrix.ml reader.ml simplex.ml
EXE = optimize.ml

all: lib exe

lib: $(LIB)
	ocamlc -o matrix.cma -a matrix.ml
	ocamlc -o reader.cma -a reader.ml
	ocamlc -o simplex.cma -a simplex.ml
	ocamlc -o instrument.cma -a instrument.ml

exe: $(EXE)
	ocamlc str.cma matrix.cma simplex.cma instrument.cma reader.cma -o optimize optimize.ml
	ocamlc str.cma matrix.cma instrument.cma -o simplex simplex.ml

clean:
	rm -f *.cmi *.cmo *.cma optimize
