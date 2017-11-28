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
  | Atom a::ts -> Succ(Pred(a,ts)::g,d,i,s)
  | Pred(a,ts1)::ts -> Succ(Pred(a,ts1@ts)::g,d,i,s)
  | _ -> Fail(d)

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

let assertz process d = function
  | Pred(":-", [t]) -> process d t
  | t               -> Vm_db.assertz d (to_db t)
let asserta process d = function
  | Pred(":-", [t]) -> process d t
  | t               -> Vm_db.asserta d (to_db t)

let consult1 process solve d t =
  let filename = match t with
  | Atom a -> a
  | Pred("library",[Atom t]) -> !libpath ^ "/" ^ t ^ ".pl"
  | _ -> failwith "loading file path error"
  in
  if !trace then Printf.printf "Loading %s\n" filename;
  let inp = open_in filename in
  let lexer = Lexing.from_channel inp in
  let rec loop d =
    match Parser.sentence Lexer.token lexer with
    | Atom "" -> d
    | p -> 
      let v = Var("_",-1) in
      let (p,d) = match solve([Pred("term_expansion",[p;v])], d, -1, []) with
        | Succ(g,d,i,s) -> (deref (e s) v,d)
        | Fail d -> (p,d)
      in
      (* goal_expansion *)
      let rec goal_expansion p d =
        match solve([Pred("goal_expansion",[p;v])], d, -1, []) with
        | Succ(g,d,i,s) -> goal_expansion (deref (e s) v) d
        | Fail d ->
          match p with
          | Pred ("," as n, [a;b]) 
          | Pred (";" as n, [a;b])
          | Pred ("->" as n, [a;b]) ->
            let a,d = goal_expansion a d in
            let b,d = goal_expansion b d in
            Pred(n, [a;b]),d
          | t            -> t,d
      in
      let p,d = match p with
      | Pred (":-",[h;goal]) -> let g,d= goal_expansion goal d in Pred(":-",[h;g]),d
      | Pred (":-",[goal]) -> let g,d= goal_expansion goal d in Pred(":-",[g]),d
      | p -> p,d
      in
      loop (assertz process d p)
  in loop d
let consult = consult1 process solve

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
  let hook = !runtime in
  runtime := fun ((g,d,i,s) as m) -> match (g,d,s) with
    | Atom "halt"         ::g, d, s -> exit 0
    | Atom "true"         ::g, d, s -> step (Succ(g,d,-1,s))
    | Atom "!"            ::g, d, (g2,e,l,_)::s -> step (Succ(g, d, -1, (g2, e,l, -2)::s))
    | Pred(",",  [u;v])   ::g, d, s -> step (Succ(u::v::g, d, -1, s))
    | Pred(";",  [u;v])   ::g, d, s -> let e,l1=el1 s in step (Succ(   u::g, d, -1, (v::g, e,l1, -1)::s))
    | Pred("=",  [u;v])   ::g, d, s -> step (uni m s u v)
    | Pred("\\=",[u;v])   ::g, d, s -> step (uninot m s u v)
    | Pred("\\", [u])     ::g, d, s -> step (not1 g d s (step(Succ([u], d, -1, []))))
    | Pred("is", [u;v])   ::g, d, s -> step (uni m s u (Num(eval (e s) (deref (e s) v))))
    | Pred("assertz", [t])::g, d, s -> step (Succ(g, assertz process d (deref (e s) t), -1, s))
    | Pred("asserta", [t])::g, d, s -> step (Succ(g, asserta process d (deref (e s) t), -1, s))
    | Pred("write",   [t])::g, d, s -> write1 (e s) t; step (Succ(g,d,-1,s))
    | Pred("retract", [t])::g, d, s -> step (Succ(g,retract1 d t (e s),-1,s))
    | Pred("retractall", [t])::g, d, s -> step (Succ(g,retractall d t (e s),-1,s))
    | Pred("consult", [t])::g, d, s -> step (Succ(g, consult1 process solve d (deref (e s) t), -1, s))
    | Pred("integer", [t])::g, d, s -> step (match deref (e s) t with Num _->Succ(g, d,-1,s)|_->pop m)
    | Pred("atom",    [t])::g, d, s -> step (match deref (e s) t with Atom _->Succ(g, d,-1,s)|_->pop m)
    | Pred("var",     [t])::g, d, s -> step (match deref (e s) t with Var _->Succ(g, d,-1,s)|_->pop m)
    | Pred("call",      t)::g, d, s -> step (call t g d (-1) s)
    | Pred("op",[p;m;ls]) ::g, d, s -> opadd s p m ls; step (Succ(g,d,-1,s))
    | Pred("=..",[a;b])   ::g, d, s -> step (univ m s a b)
    |                       g, d, s -> hook m
