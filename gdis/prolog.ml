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

let rec assert1 d = function
  | Pred(":-", [t]) -> process d t
  | t               -> Array.append d [| t |]

and consult1 d t =
  let filename = Syntax.show t in
  if !trace then Printf.printf "Loading %s\n" filename;
  let inp = open_in filename in
  let seq = Parser.seq Lexer.token (Lexing.from_channel inp) in
  List.fold_left assert1 d seq

and solve m =
  let rec step = function
  | Fail d     -> Fail d
  | Succ (g,d,i,s as m) ->
    if !trace then Printf.printf "i=%d g=[%s],e=[%s],s=%d\n"
      i (String.concat "; " (List.map Syntax.show g)) (show (e s)) (List.length s);
    match m with
    |                      [], d,  i, s -> Succ m
    |                       g, d, -2, s -> step(match pop m with Succ(g,d,i,s) when i > 0 -> Succ(g,d,-2,s)| m -> step m) 
    | Atom "halt"         ::g, d, -1, s -> exit 0
    | Atom "nop"          ::g, d, -1, s -> step (Succ(g,d,-1,s))
		| Atom "!"            ::g, d, -1, (g2,e,l,_)::s -> step (Succ(g, d, -1, (g2, e,l, -2)::s))
    | Pred(",",  [u;v])   ::g, d, -1, s -> step (Succ(u::v::g, d, -1, s))
    | Pred(";",  [u;v])   ::g, d, -1, s -> let e,l1=el1 s in step (Succ(   u::g, d, -1, (v::g, e,l1, -1)::s))
    | Pred("=",  [u;v])   ::g, d, -1, s -> step (uni m s u v)
    | Pred("\\=",  [u;v]) ::g, d, -1, s -> step (uninot m s u v)
    | Pred("is", [u;v])   ::g, d, -1, s -> step (uni m s u (Number(eval (e s) (deref (e s) v))))
    | Pred("assert",  [t])::g, d, -1, s -> step (Succ(g, assert1 d (deref (e s) t), i, s))
    | Pred("write",   [t])::g, d, -1, s -> write1 (e s) t; step (Succ(g,d,-1,s))
    | Pred("consult", [t])::g, d, -1, s -> step (Succ(g, consult1 d (deref (e s) t), i, s))
		| Pred("integer", [t])::g, d, -1, s -> step(match deref (e s) t with Number _->Succ(g, d,-1,s)|_->pop m)
		| Pred("atom", [t])::g, d, -1, s -> step(match deref (e s) t with Atom _->Succ(g, d,-1,s)|_->pop m)
		|                       g, d, -1, s -> step (Succ(g, d, 0, s))
    |                    t::g, d,  i, s ->
      if i >= Array.length d then step (pop m) else
      match d.(i) with
      | Pred(":-", [t2; t3]) ->
        let e,l1 = el1 s in
        let rec gen_t = function
          | Pred(n, ts) -> Pred(n, List.map (fun a -> gen_t a) ts)
          | Var(n, _)   -> Var(n, l1)
          | t -> t
        in
        begin match unify e t (gen_t t2) with
        | None   -> step (Succ(       t::g, d, i + 1, s))
        | Some e -> step (Succ(gen_t t3::g, d,    -1, (t::g, e, l1, i+1) :: s))
        end
      | t -> Printf.printf "Database is broken. %s\n" (Syntax.show t); Fail d
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
				if(show(e s)<>"") then Printf.printf "%s%!" (show (e s));
				if i = -2 then (Printf.printf "false\n"; d) else
				if s = [] then (Printf.printf "\ntrue\n"; d) else (
					prove false m
				)
			) else (
				if not f then Printf.printf "true\n";
				d
			)
  in prove true ([t], d, -1, [[],[],1,-2])

