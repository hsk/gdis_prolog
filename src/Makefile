all:
	rm -rf gdispl
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm -rf parser.mli
	ocamlopt str.cmxa version.ml syntax.ml parser.ml lexer.ml db.ml prolog.ml main.ml -o gdispl
	rm -rf *.cm* *.o parser.ml parser.mli lexer.ml
clean:
	rm -rf *.cm* *.o parser.ml parser.mli lexer.ml gdispl
