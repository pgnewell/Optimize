
PROG = optimize

# Setup

LIBS = \
	str.cma 

CAMLC = ocamlc
CAMLDOC = ocamldoc

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files

SOURCES = \
	helpers.ml matrix.ml instrument.ml reader.ml simplex.ml 

OBJECTS = $(SOURCES:.ml=.cmo)

# Basic Program
$(PROG): $(OBJECTS) $(PROG).cmo
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) $(PROG).cmo -o $(PROG)

build_exe: $(PROG)

test_simplex: $(OBJECTS) test.cmo 
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) -o test_simplex

clean:
	rm *.cmo *.cmi optimize test_simplex
	
