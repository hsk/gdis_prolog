type e = Num of int | Atom of string | Pred of string * e list

let rec show = function
  | Num i -> Printf.sprintf "%d" i
  | Atom s -> Printf.sprintf "%s" s
  | Pred(op,es) -> Printf.sprintf "%s(%s)" op (shows es)
and shows es = String.concat ", " (List.map show es)
let rec list = function
  | [] -> Atom("nil")
  | x::xs -> Pred("",[x;list xs])

let tokens = list[Atom(":-");Atom("a");Atom("=");Atom("b");Atom("=");Num(1);Atom("*");Num(3);Atom("+");Num(4);Atom("+");Num(5);Atom("*");Num(6)]
let prefix = function
    | ":-" -> 1200
    | _ -> 10001
let infix = function
    | "+" -> 10
    | "-" -> 10
    | "*" -> 2
    | "/" -> 2
    | _ -> 10001
let infixr = function
    | "=" -> 30
    | _ -> 10001

let rec exp_pre p = function
  | Pred("",[Atom(op);y]) when prefix((op)) <= p ->
    let (t,ts) = exp_pre (prefix((op))) y in
    (Pred(op,[t]),ts)
  | Pred("",[x;y]) -> exp_infix p x y
  | e -> (e, Atom "nil")

and exp_infix p t = function
  | Pred("",[Atom(op);y]) when infix((op)) < p ->
    let (t2,ts2) = exp_pre (infix((op))) y in
    exp_infix p (Pred(op,[t;t2])) ts2
  | Pred("",[Atom(op);y]) when infixr((op)) <= p ->
    let (t2,ts2) = exp_pre (infixr((op))) y in
    exp_infix p (Pred(op,[t;t2])) ts2
  | tokens -> (t,tokens)

let exp tokens = fst(exp_pre 10000 tokens)

let () =
  Printf.printf "%s\n" (show (exp tokens))
