open Syntax

type e = ((string * int) * t) list    (* env *)

let rec deref e t = match t with
  | Pred (n, ts) -> Pred(n, List.map (deref e) ts)
  | Var v        -> (try deref e (List.assoc v e) with _ -> t)
  | t            -> t

let show e =
  String.concat "\n" (List.fold_right (fun ((n, l), t) ls ->
    if l >= 1 then ls else (n ^ "=" ^ Syntax.show (deref e t)) :: ls
  ) e [] )

type r = e option   (* result *)

let rec unify e t t2 =
  let rec unify r (t, t2) = match r with
    | None -> None
    | Some e ->
      let rec bind t v t2 =
        try match List.assoc v e with
        | Var v as t3 -> if t == t3 then None else bind t v t2
        | t3          -> if t2== t3 then r    else mgu (t3, t2)
        with _ -> Some((v, t2) :: e)
      and mgu (t, t2) = match (t, t2) with
        |         t ,      Var v2  -> bind t2 v2 t
        |     Var v ,          t2  -> bind t v t2
        | Pred(x, g), Pred(x2, g2) -> if x <> x2 then None else
                                      (try List.fold_left unify r (List.combine g g2)
                                      with _ -> None)
        |         t ,          t2  -> if t = t2 then r else None
      in mgu (t, t2)
  in unify (Some e) (t, t2)

type g = t list             (* goals *)
type d = t array            (* database *)
type i = int                (* index *)
type s = (g * e * i * i) list (* stack *)
type m = g * d * i * s      (* gdis machine *)

type ('a, 'b) res = Fail of 'a | Succ of 'b
let trace = ref false
let interactive = ref false
let libpath = ref "/usr/share/gdispl/lib/"
let e s = match s with
  | [] -> []
  | (_, e, _, _)::_ -> e

let el1 s = match s with
  | [] -> [],1
  | (_, e, l, _)::_ -> e,l+1

let pop m = match m with
  |   _,  d, _, ( g, _,_, i)::s -> Succ (g, d, i, s)
  |   _,  d, _,              [] -> Fail d

let uni m s t t2 =
  match unify (e s) t t2, m with
  | Some e, (_::g, d, _, (sg, _,l, i)::s) -> Succ (g, d, -1, (sg, e, l, i) :: s)
  |      _,                           m -> pop m

let uninot m s t t2 =
	match unify (e s) t t2, m with
	| Some _, m -> pop m
	|      _, (_::g,d,_,s) -> Succ(g,d,-1,s)
	|      _, ([],d,_,s) -> Fail d
	
let rec eval e = function
  | Number i -> i
  | Pred("+", [x;y]) -> (eval e x) +. (eval e y)
  | Pred("*", [x;y]) -> (eval e x) *. (eval e y)
  | Pred("-", [x;y]) -> (eval e x) -. (eval e y)
  | Pred("/", [x;y]) -> (eval e x) /. (eval e y)
  | t -> failwith ("unknown term " ^ Syntax.show t)

let write1 e t = Printf.printf "%s%!" (Syntax.show (deref e t))

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
let retract1 = retract Db.retract
let retractall = retract Db.retractall

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
	| Number p, Atom a, ls -> Syntax.opadd(int_of_float p, a, ls)
	| _ -> ()

let assertz process d = function
  | Pred(":-", [t]) -> process d t
	| t               -> Db.assertz d (to_db t)
let asserta process d = function
	| Pred(":-", [t]) -> process d t
	| t               -> Db.asserta d (to_db t)

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
			let p = match solve([Pred("term_expansion",[p;v])], d, -1, []) with
				| Succ(g,d,i,s) -> deref (e s) v
				| Fail d -> p
			in
			(* goal_expansion *)
			let rec goal_expansion p =
				match solve([Pred("goal_expansion",[p;v])], d, -1, []) with
				| Succ(g,d,i,s) -> goal_expansion (deref (e s) v)
				| Fail d ->
					match p with
					| Pred ("," as n, [a;b]) 
					| Pred (";" as n, [a;b])
					| Pred ("->" as n, [a;b]) -> Pred(n, [goal_expansion a;goal_expansion b])
					| t            -> t
			in
			let p = match p with
			| Pred (":-",[h;goal]) -> Pred(":-",[h;goal_expansion goal])
			| Pred (":-",[goal]) -> Pred(":-",[goal_expansion goal])
			| p -> p
			in
			loop (assertz process d p)
	in loop d

let not1 g d s = function
	| Fail d -> Succ(g,d,-1,s)
	| Succ(_,_,_,_) -> Fail d	
let rec solve m =
  let rec step = function
  | Fail d     -> Fail d
  | Succ (g,d,i,s as m) ->
    if !trace then Printf.printf "i=%d g=[%s],e=[%s],s=%d\n"
      i (String.concat "; " (List.map Syntax.show g)) (show (e s)) (List.length s);
    match m with
    |                      [], d,  i, s -> Succ m
    |                       g, d, -2, s -> step (match pop m with Succ(g,d,i,s) when i > 0 -> Succ(g,d,-2,s)| m -> step m) 
    | Atom "halt"         ::g, d, -1, s -> exit 0
    | Atom "true"         ::g, d, -1, s -> step (Succ(g,d,-1,s))
		| Atom "!"            ::g, d, -1, (g2,e,l,_)::s -> step (Succ(g, d, -1, (g2, e,l, -2)::s))
    | Pred(",",  [u;v])   ::g, d, -1, s -> step (Succ(u::v::g, d, -1, s))
    | Pred(";",  [u;v])   ::g, d, -1, s -> let e,l1=el1 s in step (Succ(   u::g, d, -1, (v::g, e,l1, -1)::s))
    | Pred("=",  [u;v])   ::g, d, -1, s -> step (uni m s u v)
    | Pred("\\=", [u;v])  ::g, d, -1, s -> step (uninot m s u v)
    | Pred("\\", [u])     ::g, d, -1, s -> step (not1 g d s (step(Succ([u], d, -1, []))))
    | Pred("is", [u;v])   ::g, d, -1, s -> step (uni m s u (Number(eval (e s) (deref (e s) v))))
    | Pred("assertz",  [t])::g, d, -1, s -> step (Succ(g, assertz process d (deref (e s) t), -1, s))
    | Pred("asserta",  [t])::g, d, -1, s -> step (Succ(g, asserta process d (deref (e s) t), -1, s))
    | Pred("write",   [t])::g, d, -1, s -> write1 (e s) t; step (Succ(g,d,-1,s))
    | Pred("retract", [t])::g, d, -1, s -> step (Succ(g,retract1 d t (e s),-1,s))
    | Pred("retractall", [t])::g, d, -1, s -> step (Succ(g,retractall d t (e s),-1,s))
    | Pred("consult", [t])::g, d, -1, s -> step (Succ(g, consult1 process solve d (deref (e s) t), i, s))
		| Pred("integer", [t])::g, d, -1, s -> step (match deref (e s) t with Number _->Succ(g, d,-1,s)|_->pop m)
		| Pred("atom",    [t])::g, d, -1, s -> step (match deref (e s) t with Atom _->Succ(g, d,-1,s)|_->pop m)
		| Pred("call",      t)::g, d, -1, s -> step (call t g d (-1) s)
		| Pred("op",[p;m;ls]) ::g, d, -1, s -> opadd s p m ls; step (Succ(g,d,-1,s))
		|                       g, d, -1, s -> step (Succ(g, d, Db.get_start d, s))
		|                    t::g, d,  i, s ->
			if i==0 then step (pop m) else
			if Array.length d <= i then Fail d else
      match d.(i) with
      | (Pred(":-", [t2; t3]),nx) ->
        let e,l1 = el1 s in
        let rec gen_t = function
          | Pred(n, ts) -> Pred(n, List.map (fun a -> gen_t a) ts)
          | Var(n, _)   -> Var(n, l1)
          | t -> t
        in
        begin match unify e t (gen_t t2) with
        | None   -> step (Succ(       t::g, d, nx, s))
        | Some e -> step (Succ(gen_t t3::g, d,    -1, (t::g, e, l1, nx) :: s))
        end
			| (_,nx) -> step(Succ(t::g,d,nx,s))
  in
  step (match m with
    | [], _, _, _ -> pop m
    | _           -> Succ m
  )

and process d t =
  let rec prove f m = match solve m with
    | Fail d -> Printf.printf "false\n"; d
		| Succ (g, d, i, s as m) ->
			if f || show (e s) <>"" && ";" = read_line () then (
				if(!interactive && show(e s)<>"") then Printf.printf "%s%!" (show (e s));
				if i = -2 then (Printf.printf "false\n"; d) else
				if s = [] then (if !interactive then Printf.printf "\ntrue\n"; d) else (
					prove false m
				)
			) else (
				if not f && !interactive then Printf.printf "true\n";
				d
			)
  in prove true ([t], d, -1, [[],[],1,-2])

let consult = consult1 process solve