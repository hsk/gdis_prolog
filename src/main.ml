open Vm
open Vm_builtin
open Version

let welcome = "GDIS Prolog version " ^ version
let usage = "Usage: gdispl [options] file...\nOptions:"
let parse str =
  Parser.query Lexer.token (Lexing.from_string str)

let help () =
  List.iter(fun (k,v) -> Printf.printf "%s\t%s\n%!" k v)
  ["e","exit"; "l","list"; "h","help";]

let rec repl (d:(Ast.t * int) array) =
    Printf.printf("?- %!");
    match read_line () with
    | "e"  -> ()
    | "l"  -> Array.iter (fun (t,_) -> Printf.printf "%s.\n%!" (Ast.show t)) d; repl d
    | "h"  -> help (); repl d
    | "t"  -> trace := not !trace;
              Printf.printf "Tracing %s.\n%!" (if !trace then "on" else "off");
              repl d
    | line -> try repl (process d (parse line))
              with Parsing.Parse_error -> Printf.printf "Syntax error\n%!"; repl d

let () =
  let files = ref [] in
  Arg.parse (Arg.align [
      "-t", Arg.Set trace,
            " Set trace mode";
      "-v", Arg.Unit (fun ()-> Printf.printf "%s\n" welcome; exit(0)),
            " Display version infomation";
      "-lib", Arg.Set_string libpath,
            "libpath  Set libraries path";
    ])
    (fun x -> files := !files @ [x])
    usage;
  let db = ref (consult (Vm_db.empty ()) (Pred("library",[Atom "initial"]))) in (* load files *)
  !files|> List.iter(fun x ->
    db := consult !db (Atom x)
  );
  interactive := true;
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  Printf.printf "%s\n%!" welcome;
  Printf.printf "%s\n%!" (String.make (String.length welcome) '-');
  help ();
  repl !db
