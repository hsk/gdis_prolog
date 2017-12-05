open Ast
open Vm

let libpath = ref "/usr/share/gdispl/lib/"

let uninot m s t t2 =
  match unify (e s) t t2, m with
  | Some _, m -> pop m
  |      _, (_::g,d,_,s) -> Succ(g,d,-1,s)
  |      _, ([],d,_,s) -> Fail d

let rec eval e = function
  | Num i -> i
  | Pred("+", [x;y]) -> (eval e x) +. (eval e y)
  | Pred("*", [x;y]) -> (eval e x) *. (eval e y)
  | Pred("-", [x;y]) -> (eval e x) -. (eval e y)
  | Pred("/", [x;y]) -> (eval e x) /. (eval e y)
  | t -> failwith ("unknown term " ^ Ast.show t)

let write1 e t = Printf.printf "%s%!" (Ast.show (deref e t))

let call t g d i s = match List.map (fun t -> deref (e s) t) t with
  | Atom a::ts -> let e,l1=el1 s in Succ(Pred(a,ts)::g,d,i,(g,e,l1,0)::s)
  | Pred(a,ts1)::ts -> let e,l1=el1 s in Succ(Pred(a,ts1@ts)::g,d,i,(g,e,l1,0)::s)
  | p -> Fail(d)

let to_db = function
  | Pred(":-",[_;_]) as t -> t
  | t               -> Pred(":-",[t;Atom "true"])

let retract retractf d t e =
  let t = to_db t in
  retractf d (fun dt ->
    match dt,t with
    | (Pred(":-",[dt;_]),Pred(":-",[t;_])) ->
      (match unify e t dt with Some _ -> true | None -> false)
    | _ -> false
  )
let retract1 = retract Vm_db.retract
let retractall = retract Vm_db.retractall

let rec to_list = function
  | Atom "[]" -> []
  | Pred("[|]",[a;b]) -> a::to_list b
  | c -> [c]

let opadd s p a ls =
  let ls = List.map(fun a->
    match deref (e s) a with
    | Atom a -> a
    | _ -> assert false
  ) (to_list ls) in
  match deref (e s) p, deref (e s) a, ls with
  | Num p, Atom a, ls -> Ast.opadd(int_of_float p, a, ls)
  | _ -> ()

let assertz d t = Vm_db.assertz d (to_db t)
let asserta d t = Vm_db.asserta d (to_db t)
let current_predicate = fun [u] g d s m ->
  let p = (Ast.show (deref (e s) u)) in
  if List.mem_assoc p !builtins
  || Array.to_list d |>List.exists(function
    | (Pred(":-",[u;v]),_)->p=arity u
    |(p,_)->false )
  then Succ(g,d,-1,s) else Fail d

let read d t =
  let filename = match t with
  | Atom a -> a
  | Pred("library",[Atom t]) -> !libpath ^ "/" ^ t ^ ".pl"
  | _ -> failwith "loading file path error"
  in
  if !trace then Printf.printf "Loading %s\n" filename;
  let inp = open_in filename in
  let lexer = Lexing.from_channel inp in
  let rec loop () =
    match Parser.sentence Lexer.token lexer with
    | Atom "" -> Atom "[]"
    | p -> Pred("[|]",[Vm_db.copy_term p;loop ()])
  in
  let ps = loop () in
  close_in inp;
  ps
let read2 m g d s t u =
  let ps = read d (deref (e s)t) in
  uni m s u ps

let consult d t =
  match solve1 d (Pred("current_predicate",[Pred("/",[Atom"consult";Num 1.0])])) with
  | Fail d -> let rec loop d = function
                | Atom "[]" -> d
                | Pred("[|]",[p;ps]) -> loop (assertz d (Parser.opconvert p)) ps
              in
              loop d (read d t)
  | Succ(g,d,i,s)->
    match solve1 d (Pred("consult",[t])) with
    | Succ(g,d,i,s) -> d
    | Fail d -> Printf.printf "false\n"; d

let not1 g d s = function
  | Fail d -> Succ(g,d,-1,s)
  | Succ(_,_,_,_) -> Fail d

let univ m s a b =
  let rec list2pred = function
    | [] -> Atom "[]"
    | x::xs -> Pred("[|]",[x;list2pred xs])
  in
  let rec pred2list = function
    | Atom "[]" -> [] 
    | Pred("[|]",[x; xs]) -> x::pred2list xs
  in
  match deref (e s) a, deref (e s) b,m with
  | Var _,Var _, (_,d,_,_) -> Fail d
  | (Var _) as t, Pred("[|]",[Atom a;b]), m ->
    uni m s t (Pred(a,pred2list b))
  | Pred(a,ts), t, m -> uni m s t (Pred("[|]",[Atom a;list2pred ts]))
  | Atom "[]", t, m -> uni m s t (Pred("[|]",[Atom "[]";Atom"[]"]))
  | _,_,(_,d,_,_) -> Fail d

let () =
  builtins := [
  "halt/0"         , (fun [] g d s m -> exit 0; Fail d);
  "true/0"         , (fun [] g d s m -> Succ(g,d,-1,s));
  "!/0"            , (fun [] g d s m -> match s with (g2,e,l,_)::s -> Succ(g, d, -1, (g2, e,l, -2)::s) | _->Fail d);
  ",/2"            , (fun [u;v] g d s m -> Succ(u::v::g, d, -1, s));
  ";/2"            , (fun [u;v] g d s m -> let e,l1=el1 s in Succ(u::g, d, -1, (v::g, e,l1, -1)::s));
  "=/2"            , (fun [u;v] g d s m -> uni m s u v);
  "\\=/2"          , (fun [u;v] g d s m -> uninot m s u v);
  "\\+/1"          , (fun [u] g d s m -> not1 g d s (step(Succ([u], d, -1, []))));
  "is/2"           , (fun [u;v] g d s m -> uni m s u (Num(eval (e s) (deref (e s) v))));
  "assertz/1"      , (fun [t] g d s m -> Succ(g, assertz d (deref (e s) t), -1, s));
  "asserta/1"      , (fun [t] g d s m -> Succ(g, asserta d (deref (e s) t), -1, s));
  "write/1"        , (fun [t] g d s m -> write1 (e s) t; Succ(g,d,-1,s));
  "retract/1"      , (fun [t] g d s m -> Succ(g,retract1 d t (e s),-1,s));
  "retractall/1"   , (fun [t] g d s m -> Succ(g,retractall d t (e s),-1,s));
  "read/2"         , (fun [t;u] g d s m -> read2 m g d s t u);
  "read/1"         , (fun [t] g d s m -> Succ(g, consult d (deref (e s) t), -1, s));
  "integer/1"      , (fun [t] g d s m -> (match deref (e s) t with Num _->Succ(g, d,-1,s)|_->pop m));
  "atom/1"         , (fun [t] g d s m -> (match deref (e s) t with Atom _->Succ(g, d,-1,s)|_->pop m));
  "var/1"          , (fun [t] g d s m -> (match deref (e s) t with Var _->Succ(g, d,-1,s)|_->pop m));
  "op/3"           , (fun [p;m;ls] g d s _ -> opadd s p m ls; Succ(g,d,-1,s));
  "=../2"          , (fun [a;b] g d s m -> univ m s a b);
  "call/1"         , (fun t g d s m -> call t g d (-1) s);
  "call/2"         , (fun t g d s m -> call t g d (-1) s);
  "call/3"         , (fun t g d s m -> call t g d (-1) s);
  "call/4"         , (fun t g d s m -> call t g d (-1) s);
  "call/5"         , (fun t g d s m -> call t g d (-1) s);
  "call/6"         , (fun t g d s m -> call t g d (-1) s);
  "call/7"         , (fun t g d s m -> call t g d (-1) s);
  "call/8"         , (fun t g d s m -> call t g d (-1) s);
  "opconvert/2"    , (fun [u;v] g d s m -> let u = Parser.opconvert (deref (e s) u) in uni m s (deref (e s) u) (deref (e s) v) );
  "current_predicate/1", current_predicate;
  ]
