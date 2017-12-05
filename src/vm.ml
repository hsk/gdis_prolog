open Ast

type e = ((string * int) * t) list    (* env *)

let rec deref e t = match t with
  | Pred (n, ts) -> Pred(n, List.map (deref e) ts)
  | Var v        -> (try deref e (List.assoc v e) with _ -> t)
  | t            -> t

let show e =
  (List.fold_right (fun ((n, l), t) ls ->
    if l >= 1 then ls else (n ^ "=" ^ Ast.show (deref e t)^"\n") ^ ls
  ) e "" )

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
type d = (t * int)array     (* database *)
type i = int                (* index *)
type s = (g * e * i * i) list (* stack *)
type m = g * d * i * s      (* gdis machine *)

type ('a, 'b) res = Fail of 'a | Succ of 'b
let trace = ref false
let interactive = ref false
let e s = match s with
  | [] -> []
  | (_, e, _, _)::_ -> e

let el1 s = match s with
  | [] -> [],1
  | (_, e, l, _)::_ -> e,l+1

let pop m = match m with
|   _,  d, _,              [] -> Fail d
|   _,  d, _, ( g, _,_, i)::s -> Succ (g, d, i, s)
let rec pop1 l1 d s = match s with
| []->Fail d
| (g,e,l,i)::s when l1=l -> pop1 l1 d s
| (g,e,l,i)::s -> Succ(g,d,0, s)

let backtrack d s = match s with
| []->Fail d
| (_,e,l,i)::s -> pop1 l d s
let uni m s t t2 =
  match unify (e s) t t2, m with
  | Some e, (_::g, d, _, (sg, _,l, i)::s) -> Succ (g, d, -1, (sg, e, l, i) :: s)
  |      _,                           m -> pop m

let arity = function
  | Atom a -> a^"/0"
  | Pred(a,ts)-> a^"/"^(string_of_int (List.length ts))
  | _ -> "none"
let params = function
  | Atom a -> []
  | Pred(a,ts)-> ts
  | _ -> assert false

let builtins = ref []

let step = function
  | Fail d     -> Fail d
  | Succ (g,d,i,s as m) ->
    if !trace then Printf.printf "i=%d g=[%s],d=%d,e=[%s],s=%d\n%!"
      i (String.concat "; " (List.map Ast.show g)) (Array.length d) (show (e s)) (List.length s);
    match m with
    |            [],d, i,s -> Succ m
    |             g,d,-2,s -> backtrack d s
    | Pred(a,[])::g,d, i,s -> Succ(Atom a::g,d,i,s)
    |          t::g,d,-1,s when List.mem_assoc (arity t) !builtins -> (List.assoc (arity t) !builtins) (params t) g d s m
    |             g,d,-1,s -> Succ(g, d, Vm_db.get_start d, s)
    |          t::g,d, i,s ->
      let rec loop i =
        if i=0 || Array.length d = 4 then pop m else (* todo この Array.length d = 4 は消したい *)
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
          | None   -> loop nx
          | Some e -> Succ(gen_t t3::g, d,    -1, (t::g, e, l1, nx) :: s)
          end
        | (_,nx) -> loop nx
      in loop i
(* マシンを受取り、ステップ実行を停止状態まで動かし、マシンを返す *)
let rec solve m =
  let m = match m with
    | [], _, _, _ -> pop m
    | _           -> Succ m
  in
  match step m with
  | (Succ ([],d,i,s) as m) ->
    if !trace then Printf.printf "i=%d g=[],d=%d,e=[%s],s=%d\n%!"
      i (Array.length d) (show (e s)) (List.length s);
    m
  | Succ m -> solve m
  | Fail d -> Fail d

let rec prove m = match solve m with
  | Fail d -> Printf.printf "false\n%!"; d
  | Succ (g, d, i, s as m) -> d

let rec iprove m = match solve m with
  | Fail d -> Printf.printf "false\n%!"; d
  | Succ (g, d, i, s as m) ->
    Printf.printf "%s%!" (show (e s));
    if List.length s <= 1 then d else 
    if ";" <> read_line () then (Printf.printf "true\n%!"; d) else
    iprove m

(* DBと項を受取って、マシンの初期設定をして実行し結果のDBを返す *)
let process d t =
  if !interactive then iprove ([t], d, -1, [[],[],1,-2])
  else prove ([t], d, -1, [[],[],1,-2])

let solve1 d t = solve ([t],d,-1,[[],[],1,-2])
