LIB = matrix.ml
EXE = simplex.ml

all: lib exe

lib: $(LIB)
	ocamlc -a matrix.ml -o matrix.cma

exe: $(EXE)
	ocamlc -o simplex matrix.cma simplex.ml

clean:
	rm -f *.cmi *.cmo *.cma simplex
