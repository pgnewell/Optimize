LIB = matrix.ml
EXE = simplex.ml

lib: $(LIB)
	ocamlc -a matrix.ml -o matrix.cma
exe: $(EXE)
	ocamlc matrix.cma simplex.ml
clean:
	rm -f *.cmi *.cmo *.cma simplex
