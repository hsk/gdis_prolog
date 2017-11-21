type x = string

type t =
  | Atom of x
  | Number of float
  | Str of x
  | Pred of x * t list
  | Var of (x * int)

let default_ops = [
    600, "xfy", ["::";"as"];
    910, "xfx", ["/-";"\\-"];
    920, "xfx", [ "==>"; "==>>";"<:" ];

    1300,	"xf",	["."]; (* added *)
    1200,	"xfx",	["-->"; ":-"; "::="(*added*)];
    1190,	"fx",	[":-"; "?-"];
    1150,	"fx",	["dynamic"; "discontiguous"; "initialization"; "meta_predicate";"module_transparent"; "multifile"; "public"; "thread_local";"thread_initialization"; "volatile"];
    1100,	"xfy",	[";"];
    (*1100,	"xfy",	[";"; "|"];*)
    1050,	"xfy",	["->"; "*->"];
    1000,	"xfy",	[","];

    995,  "xfy",  ["|"];(* change *)

    990,	"xfx",	[":="];
    900,	"fy",	["\\+"];
    700,	"xfx",	["<"; "="; "=.."; "=@="; "\\=@="; "=:="; "=<"; "=="; "=\\="; ">"; ">="; "@<"; "@=<"; "@>"; "@>="; "\\="; "\\=="; "as"; "is"; ">:<"; ":<"];
    600,	"xfy",	[":"];
    500,	"yfx",	["+"; "-"; "/\\"; "\\/"; "xor"];
    500,	"fx",	["?"];
    400,	"yfx",	["*"; "/"; "//"; "div"; "rdiv"; "<<"; ">>"; "mod"; "rem"];
    200,	"xfx",	["**"];
    200,	"xfy",	["^"];
    200,	"fy",	["+"; "-"; "\\"];
(*    100,	"yfx",	["."];*)
    1,	"fx",	["$"];
  ]

let opsmap = ref []

let rec update x v = function
  | [] -> [x,v]
  | (k,_)::xs when x=k -> (x,v)::xs
  | (k,v1)::xs -> (k,v1)::update x v xs

let opadd(p,o,(ls : string list)) =
  List.iter(fun (op:string) ->
    let map = (op,p)::List.assoc o !opsmap in
    opsmap := update o map !opsmap
  ) ls

let init_ops() =
  opsmap := ["xfx",[];"yfx",[];"fx",[];"fy",[];"xfy",[];"xf",[];"yf",[]];
  List.iter opadd default_ops

let () = init_ops()

let opn o op =
  try List.assoc op (List.assoc o !opsmap)
  with _ -> -1

let opnXfy op =
  try List.assoc op (List.assoc "xfy" !opsmap)
  with _ -> opn "xfx" op

let opnXf op =
  try List.assoc op (List.assoc "xf" !opsmap)
  with _ -> opn "yf" op

let opnFx op =
  try List.assoc op (List.assoc "fx" !opsmap)
  with _ -> opn "fy" op

let is_upper x =
  if x = "" then false else
  let c = Char.code (String.get x 0) in
  (c >= Char.code 'A' && c <= Char.code 'Z' || c == Char.code '_')

let escaped x =
  let reg = Str.regexp "'" in
  let x = String.escaped x in
  Str.global_replace reg "\\'" x

let rec show1 = function
  | Atom(x)           -> Printf.sprintf "Atom(%s)" (show_atom x)
  | Number(n)         -> Printf.sprintf "Number(%f)" n
  | Str(x)            -> Printf.sprintf "Str(%S)" x
  | Pred(x, xs)       -> Printf.sprintf "Pred(%s,[%s])" (show_atom x) (show1s xs)
  | Var(x,i)          -> Printf.sprintf "Var(%s,%d)" x i
and show1s ls =
  String.concat ", " (List.map (fun e-> show1 e) ls)
and show_atom x = Printf.sprintf "%S" x

let show_atom x =
  if is_upper x then Printf.sprintf "'%s'" (escaped x) else x

let showbin = function
  | "," -> ", "
  | "!" -> "!"
  | x -> " " ^ x ^ " "

let rec show p = function
  | Pred("",[t1;t2])   -> Printf.sprintf "%s %s"  (show 10000 t1) (show 10000 t2)
  | Pred(x,[t1;t2]) when opn "yfx" x >= p ->
    let p2 = opn "yfx" x in
    Printf.sprintf "(%s%s%s)"  (show (p2+1) t1) (showbin x) (show (p2-0) t2)
  | Pred(x,[t1;t2]) when opn "yfx" x >= 0 ->
    let p2 = opn "yfx" x in
    Printf.sprintf "%s%s%s" (show (p2+1) t1) (showbin x) (show (p2-0) t2)
  | Pred(x,[t1;t2]) when opnXfy x >= p ->
    let p2 = opnXfy x in
    Printf.sprintf "(%s%s%s)"  (show p2 t1) (showbin x) (show (p2+1) t2)
  | Pred(x,[t1;t2]) when opnXfy x >= 0 ->
    let p2 = opnXfy x in
    Printf.sprintf "%s%s%s" (show p2 t1) (showbin x) (show (p2+1) t2)
  | Atom("!")           -> "!"
  | Atom("[]")          -> "[]"
  | Atom(x)             -> show_atom x
	| Number(n)           -> let s = string_of_float n in let l = (String.length s)-1 in
													 if String.sub s l 1 = "." then String.sub s 0 l else s
  | Str(x)              -> Printf.sprintf "%s" x
  | Pred(x, [xs]) when opnFx x >= p -> let p = opnFx x in Printf.sprintf "(%s %s)" x (show p xs)
  | Pred(x, [xs]) when opnFx x >= 0 -> let p = opnFx x in Printf.sprintf "%s %s" x (show p xs)
  | Pred(".",[xs])      -> Printf.sprintf "%s.\n" (show p xs)
  | Pred(",",[t1;t2])   -> Printf.sprintf "%s, %s"  (show p t1) (show p t2)
	| Pred("{}", xs)      -> Printf.sprintf "{%s}" (shows xs)
  | Pred("[|]", _) as t -> Printf.sprintf "[%s]" (show_list t)
  | Pred(x, xs)         -> Printf.sprintf "%s(%s)" (show_atom x) (shows xs)
  | Var(x,i)            -> Printf.sprintf "%s_%d" x i
and show_list = function
  | Pred("[|]", [t; Atom("[]")])         -> show (opn "xfy" ",") t
  | Pred("[|]", [t;Pred("[|]", _) as u]) -> show (opn "xfy" ",") t ^ "," ^ show_list u
  | Pred("[|]", [t;u])                   -> show (opn "xfy" ",") t ^ "|" ^ show (opn "xfy" ",") u
  | t                                    -> show (opn "xfy" ",") t
and shows ls =
  String.concat ", " (List.map (fun e-> show (opn "xfy" ",") e) ls)
let show: t -> string = show 10002
let opn o op =
	try List.assoc op (List.assoc o !opsmap)
	with _ -> 10001

let prefixs op =
	try List.assoc op (List.assoc "fx" !opsmap)
	with _ -> opn "fy" op
	
let infixrs op =
  try List.assoc op (List.assoc "xfy" !opsmap)
  with _ ->
  (try List.assoc op (List.assoc "xfx" !opsmap)
  with _ -> 10001)

let infixs op =
  try List.assoc op (List.assoc "yfx" !opsmap)
  with _ -> 10001

let postfixs op =
  try List.assoc op (List.assoc "xf" !opsmap)
  with _ -> opn "yf" op

let rec exp_pre p = function
	| Pred("",[Atom(op);y]) when prefixs op <= p ->
		let (t,ts) = exp_pre (prefixs op) y in
		(Pred(op,[t]),ts)
	| Pred("",[Pred("",xs);y]) ->
		let (t,ts) = (exp_pre 10000 (Pred("",xs))) in
		exp_infix p t y
	| Pred("",[x;y]) -> exp_infix p (exp x) y
	| Pred(a,s) -> (Pred(a,List.map exp s),Atom"")
	| e -> (e, Atom "")

and exp_infix p t = function
	| Pred("",[Atom(op);y]) when infixs op < p ->
		let (t2,ts2) = exp_pre (infixs((op))) y in
		exp_infix p (Pred(op,[t;t2])) ts2
	| Pred("",[Atom(op);y]) when infixrs op <= p ->
		let (t2,ts2) = exp_pre (infixrs op) y in
		exp_infix p (Pred(op,[t;t2])) ts2
	| Pred("",[Atom(op);y]) when postfixs op <= p ->
		exp_infix p (Pred(op,[t])) y
	| tokens -> (t,tokens)
and exp e =
  let a = exp_pre 10000 e in
  fst a
