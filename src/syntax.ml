type x = string

type t =
  | Atom of x
  | Number of float
  | Str of x
  | Pred of x * t list
  | Var of (x * int)

type opp = Xfx|Yfx|Fx|Fy|Xfy|Xf|Yf

let show_o = function
  |Xfx -> "xfx"
  |Yfx -> "xyx"
  |Fx -> "fx"
  |Fy -> "fy"
  |Xfy -> "xfy"
  |Xf -> "xf"
  |Yf -> "yf"


let default_ops = [
    600, Xfy, ["::";"as"];
    910, Xfx, ["/-";"\\-"];
    920, Xfx, [ "==>"; "==>>";"<:" ];

    1300,	Xf,	["."]; (* added *)
    1200,	Xfx,	["-->"; ":-"; "::="(*added*)];
    1190,	Fx,	[":-"; "?-"];
    1150,	Fx,	["dynamic"; "discontiguous"; "initialization"; "meta_predicate";"module_transparent"; "multifile"; "public"; "thread_local";"thread_initialization"; "volatile"];
    1100,	Xfy,	[";"];
    (*1100,	Xfy,	[";"; "|"];*)
    1050,	Xfy,	["->"; "*->"];
    1000,	Xfy,	[","];

    995,  Xfy,  ["|"];(* change *)

    990,	Xfx,	[":="];
    900,	Fy,	["\\+"];
    700,	Xfx,	["<"; "="; "=.."; "=@="; "\\=@="; "=:="; "=<"; "=="; "=\\="; ">"; ">="; "@<"; "@=<"; "@>"; "@>="; "\\="; "\\=="; "as"; "is"; ">:<"; ":<"];
    600,	Xfy,	[":"];
    500,	Yfx,	["+"; "-"; "/\\"; "\\/"; "xor"];
    500,	Fx,	["?"];
    400,	Yfx,	["*"; "/"; "//"; "div"; "rdiv"; "<<"; ">>"; "mod"; "rem"];
    200,	Xfx,	["**"];
    200,	Xfy,	["^"];
    200,	Fy,	["+"; "-"; "\\"];
(*    100,	Yfx,	["."];*)
    1,	Fx,	["$"];
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
  opsmap := [Xfx,[];Yfx,[];Fx,[];Fy,[];Xfy,[];Xf,[];Yf,[]];
  List.iter opadd default_ops

let () = init_ops()

let show_opp (op,p) = Printf.sprintf "%S,%d" op p

let show_opps () =
  !opsmap |> List.iter(fun (o,ls)->
    Printf.printf "%s : " (show_o o);
    let ls = List.map show_opp ls in
    Printf.printf "[%s]\n" (String.concat "; " ls)
  )

let opn o op =
  try List.assoc op (List.assoc o !opsmap)
  with _ -> -1

let opnXfy op =
  try List.assoc op (List.assoc Xfy !opsmap)
  with _ -> opn Xfx op

let opnXf op =
  try List.assoc op (List.assoc Xf !opsmap)
  with _ -> opn Yf op

let opnFx op =
  try List.assoc op (List.assoc Fx !opsmap)
  with _ -> opn Fy op

let is_lower x =
  if x = "" then false else
  let c = Char.code (String.get x 0) in
  (c >= Char.code 'a' && c <= Char.code 'z')

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
  if is_lower x then x else Printf.sprintf "'%s'" (escaped x)

let showbin = function
  | "," -> ", "
  | "!" -> "!"
  | x -> " " ^ x ^ " "

let rec show p = function
  | Pred("",[t1;t2])   -> Printf.sprintf "%s %s"  (show 10000 t1) (show 10000 t2)
  | Pred(x,[t1;t2]) when opn Yfx x >= p ->
    let p2 = opn Yfx x in
    Printf.sprintf "(%s%s%s)"  (show (p2+1) t1) (showbin x) (show (p2-0) t2)
  | Pred(x,[t1;t2]) when opn Yfx x >= 0 ->
    let p2 = opn Yfx x in
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
  | Str(x)              -> Printf.sprintf "%S" x
  | Pred(x, [xs]) when opnFx x >= p -> let p = opnFx x in Printf.sprintf "(%s %s)" x (show p xs)
  | Pred(x, [xs]) when opnFx x >= 0 -> let p = opnFx x in Printf.sprintf "%s %s" x (show p xs)
  | Pred(".",[xs])      -> Printf.sprintf "%s.\n" (show p xs)
  | Pred(",",[t1;t2])   -> Printf.sprintf "%s, %s"  (show p t1) (show p t2)
	| Pred("{}", xs)      -> Printf.sprintf "{%s}" (shows xs)
  | Pred("[|]", _) as t -> Printf.sprintf "[%s]" (show_list t)
  | Pred(x, xs)         -> Printf.sprintf "%s(%s)" (show_atom x) (shows xs)
  | Var(x,i)            -> Printf.sprintf "%s_%d" x i
and show_list = function
  | Pred("[|]", [t; Atom("[]")])         -> show (opn Xfy ",") t
  | Pred("[|]", [t;Pred("[|]", _) as u]) -> show (opn Xfy ",") t ^ "," ^ show_list u
  | Pred("[|]", [t;u])                   -> show (opn Xfy ",") t ^ "|" ^ show (opn Xfy ",") u
  | t                                    -> show (opn Xfy ",") t
and shows ls =
  String.concat ", " (List.map (fun e-> show (opn Xfy ",") e) ls)
let show: t -> string = show 10002
let opn o op =
	try List.assoc op (List.assoc o !opsmap)
	with _ -> 10001

let prefixs op =
	try List.assoc op (List.assoc Fx !opsmap)
	with _ -> opn Fy op
	
let infixrs op =
  try List.assoc op (List.assoc Xfy !opsmap)
  with _ ->
  (try List.assoc op (List.assoc Xfx !opsmap)
  with _ -> 10001)

let infixs op =
  try List.assoc op (List.assoc Yfx !opsmap)
  with _ -> 10001

let postfixs op =
  try List.assoc op (List.assoc Xf !opsmap)
  with _ -> opn Yf op

let rec exp_pre p = function
	| Pred("",[Atom(op);y]) when prefixs op <= p ->
		let (t,ts) = exp_pre (prefixs op) y in
		(Pred(op,[t]),ts)
	| Pred("",[Pred("",xs);y]) ->
		let (t,ts) = (exp_pre 10000 (Pred("",xs))) in
		exp_infix p t y
	| Pred("",[x;y]) -> exp_infix p x y
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
