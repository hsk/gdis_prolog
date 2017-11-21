open Syntax

let parse_file filename = 
	let fp = open_in filename in
	let lexer = Lexing.from_channel fp in
	let rec loop l =
		match Parser.sentence Lexer.token lexer with
		| Atom "" -> l
		| t -> loop (t::l)
	in List.rev(loop [])

let () =
	let ls = parse_file "test.pl" in
	List.iter(fun ast ->
		Printf.printf "%s.\n" (show1 ast)
	) ls;
	List.iter(fun ast ->
		Printf.printf "%s.\n" (show ast)
	) ls
