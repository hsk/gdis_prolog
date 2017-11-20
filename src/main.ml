open Syntax
open Prolog
open Version

let welcome = "GDIS Prolog version " ^ version

let parse str =
  Parser.query Lexer.token (Lexing.from_string str)

let help () =
  List.iter(fun (k,v) -> Printf.printf "%s\t%s\n%!" k v)
  ["e","exit"; "l","list"; "h","help";]

let rec repl (d:(Syntax.t * int) array) =
    Printf.printf("?- %!");
    match read_line () with
    | "e"  -> ()
    | "l"  -> Array.iter (fun (t,_) -> Printf.printf "%s.\n%!" (Syntax.show t)) d; repl d
    | "h"  -> help (); repl d
    | "t"  -> trace := not !trace;
              Printf.printf "Tracing %s.\n%!" (if !trace then "on" else "off");
              repl d
    | line -> try repl (process d (parse line))
              with Parsing.Parse_error -> Printf.printf "Syntax error\n%!"; repl d

let () =
	let files = ref [] in
  Arg.parse
    ["-t", Arg.Set trace, "trace";"-lib", Arg.Set_string libpath, "<libpath> set libraries path";]
    (fun x -> files := !files @ [x])
		"Usage: gdispl <options> filename1 filename2 ...\noptions are:";
	let db = ref (consult1 (Db.empty ()) (Pred("library",[Atom "initial"]))) in (* load files *)
	!files|> List.iter(fun x ->
		db := consult1 !db (Atom x)
	);
	interactive := true;
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  Printf.printf "%s\n%!" welcome;
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  help ();
  repl !db
