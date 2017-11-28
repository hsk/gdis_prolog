all:
	rm -rf gdispl
	ocamlyacc parser.mly
	ocamllex lexer.mll
	rm -rf parser.mli
	ocamlopt str.cmxa version.ml ast.ml parser.ml lexer.ml vm_db.ml vm.ml vm_builtin.ml main.ml -o gdispl
	rm -rf *.cm* *.o parser.ml parser.mli lexer.ml
clean:
	rm -rf *.cm* *.o parser.ml parser.mli lexer.ml gdispl
