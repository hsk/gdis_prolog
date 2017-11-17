open Syntax
open Prolog

let welcome = "GDIS Prolog version 0.0.2"

let parse str =
  Parser.query Lexer.token (Lexing.from_string str)

let help () =
  List.iter(fun (k,v) -> Printf.printf "%s\t%s\n%!" k v)
  ["e","exit"; "l","list"; "h","help";]

let rec repl d =
    Printf.printf("?- %!");
    match read_line () with
    | "e"  -> ()
    | "l"  -> Array.iter (fun t -> Printf.printf "%s.\n%!" (Syntax.show t)) d; repl d
    | "h"  -> help (); repl d
    | "t"  -> trace := not !trace;
              Printf.printf "Tracing %s.\n%!" (if !trace then "on" else "off");
              repl d
    | line -> try repl (process d (parse line))
              with Parsing.Parse_error -> Printf.printf "Syntax error\n%!"; repl d

let () =
  let db = ref (consult1 [||] (Atom "lib/initial.pl")) in (* load files *)
  Arg.parse
    ["-t", Arg.Set trace, "trace";]
    (fun x -> db := consult1 !db (Atom x))
    "Usage: bpij [-t] filename1 filename2 ...";
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  Printf.printf "%s\n%!" welcome;
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  help ();
  repl !db
