type x = string

type t =
  | Atom of x
  | Num of float
  | Str of x
  | Pred of x * t list
  | Var of (x * int)

let default_ops = [
  1300, "xf",  ["."]; (* added *)
  1200, "xfx", ["-->"; ":-"];
  1190, "fx",  [":-"; "?-"];
  1150, "fx",  ["dynamic"; "discontiguous"; "initialization"; "meta_predicate";
                "module_transparent"; "multifile"; "public"; "thread_local";
                "thread_initialization"; "volatile"];
  1100, "xfy", [";"];
  (*1100, "xfy", [";"; "|"];*)
  1050, "xfy", ["->"; "*->"];
  1000, "xfy", [","];
  995,  "xfy", ["|"];(* change *)
  990,  "xfx", [":="];
  900,  "fy",  ["\\+"];
  700,  "xfx", ["<"; "="; "=.."; "=@="; "\\=@="; "=:="; "=<"; "=="; "=\\=";
                ">"; ">="; "@<"; "@=<"; "@>"; "@>="; "\\="; "\\=="; "as";
                "is"; ">:<"; ":<"];
  600,  "xfy", [":"];
  500,  "yfx", ["+"; "-"; "/\\"; "\\/"; "xor"];
  500,  "fx",  ["?"];
  400,  "yfx", ["*"; "/"; "//"; "div"; "rdiv"; "<<"; ">>"; "mod"; "rem"];
  200,  "xfx", ["**"];
  200,  "xfy", ["^"];
  200,  "fy",  ["+"; "-"; "\\"];
  (*100, "yfx", ["."];*)
  1,    "fx",  ["$"];
]

(* operator perecedences *)
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

let _ = init_ops ()

(* show *)

let rec show1 = function
  | Atom(x)     -> Printf.sprintf "Atom(%S)" x
  | Num(n)   -> Printf.sprintf "Num(%f)" n
  | Str(x)      -> Printf.sprintf "Str(%S)" x
  | Pred(x, xs) -> Printf.sprintf "Pred(%S,[%s])" x (show1s xs)
  | Var(x,i)    -> Printf.sprintf "Var(%s,%d)" x i
and show1s ls =
  String.concat ", " (List.map show1 ls)

let rec opn defaultp os op =
  match os with
  | [] -> defaultp
  | o::os ->
    try List.assoc op (List.assoc o !opsmap)
    with Not_found -> opn defaultp os op

let opnFx  = opn (-1) ["fx";"fy"]
let opnXf  = opn (-1) ["xf";"yf"]
let opnXfy = opn (-1) ["xfy";"xfx"]
let opnYfx = opn (-1) ["yfx"]

let is_upper x =
  if x = "" then false else
  let c = Char.code (String.get x 0) in
  (c >= Char.code 'A' && c <= Char.code 'Z' || c == Char.code '_')

let escaped x =
  let reg = Str.regexp "'" in
  let x = String.escaped x in
  Str.global_replace reg "\\'" x

let show_atom x =
  if is_upper x then Printf.sprintf "'%s'" (escaped x) else x

let showbin = function
  | "," -> ", "
  | "!" -> "!"
  | x -> " " ^ x ^ " "

let rec show p = function
  | Pred("",[t1;t2])   -> Printf.sprintf "%s %s"  (show 10000 t1) (show 10000 t2)
  | Pred(x,[t1;t2]) when opnYfx x >= 0 ->
    let p2 = opnYfx x in
    let s = Printf.sprintf "%s%s%s" (show (p2+1) t1) (showbin x) (show p2 t2) in
    if p2 >= p then "("^s^")" else s
  | Pred(x,[t1;t2]) when opnXfy x >= 0 ->
    let p2 = opnXfy x in
    let s = Printf.sprintf "%s%s%s" (show p2 t1) (showbin x) (show (p2+1) t2) in
    if p2 >= p then "("^s^")" else s
  | Atom("!")           -> "!"
  | Atom("[]")          -> "[]"
  | Atom(x)             -> show_atom x
  | Num(n)           -> let s = string_of_float n in let l = (String.length s)-1 in
                           if String.sub s l 1 = "." then String.sub s 0 l else s
  | Str(x)              -> Printf.sprintf "%S" x
  | Pred(x, [xs]) when opnFx x >= p -> Printf.sprintf "(%s %s)" x (show (opnFx x) xs)
  | Pred(x, [xs]) when opnFx x >= 0 -> Printf.sprintf "%s %s" x (show (opnFx x) xs)
  | Pred(".",[xs])      -> Printf.sprintf "%s.\n" (show p xs)
  | Pred(",",[t1;t2])   -> Printf.sprintf "%s, %s"  (show p t1) (show p t2)
  | Pred("{}", xs)      -> Printf.sprintf "{%s}" (shows xs)
  | Pred("[|]", _) as t -> Printf.sprintf "[%s]" (show_list t)
  | Pred(x, xs)         -> Printf.sprintf "%s(%s)" (show_atom x) (shows xs)
  | Var(x,i)            -> Printf.sprintf "%s_%d" x i
and show_list = function
  | Pred("[|]", [t; Atom("[]")])         -> show (opnXfy ",") t
  | Pred("[|]", [t;Pred("[|]", _) as u]) -> show (opnXfy ",") t ^ "," ^ show_list u
  | Pred("[|]", [t;u])                   -> show (opnXfy ",") t ^ "|" ^ show (opnXfy ",") u
  | t                                    -> show (opnXfy ",") t
and shows ls =
  String.concat ", " (List.map (fun e-> show (opnXfy ",") e) ls)
let show: t -> string = show 10000
