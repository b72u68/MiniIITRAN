FILES=varmap.ml ast.ml parser.mli lexer.ml parser.ml interp.mli interp.ml print.mli print.ml typecheck.ml iitran.ml
BINS=iitran

all: $(BINS)

iitran: lexer.ml $(FILES)
	ocamlopt -o iitran $(FILES)

parser: parser.mly
	menhir parser.mly

lexer.ml: parser lexer.mll
	ocamllex lexer.mll

.PHONY: submit
submit: parser.mly interp.ml
	@[ -d "submit" ] && rm -rf submit
	@[ ! -d "submit" ] && mkdir submit
	cp -rf parser.mly interp.ml submit


test: tests runtest.py
	python3 runtest.py

clean:
	rm -f *~
	rm -f *.cmo
	rm -f *.cmi
	rm -f *.cmx
	rm -f *.o
	rm -f $(BINS)
	rm -f parser.ml
	rm -f parser.mli
	rm -f lexer.ml
	rm -f parser.automaton
	rm -f parser.conflicts
