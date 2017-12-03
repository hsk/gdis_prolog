open Ast

type e = ((string * int) * t) list    (* env *)

let rec deref e t = match t with
  | Pred (n, ts) -> Pred(n, List.map (deref e) ts)
  | Var v        -> (try deref e (List.assoc v e) with _ -> t)
  | t            -> t

let show e =
  String.concat "\n" (List.fold_right (fun ((n, l), t) ls ->
    if l >= 1 then ls else (n ^ "=" ^ Ast.show (deref e t)) :: ls
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

let runtime = ref (fun (g,d,i,s)->Fail d)

let rec step = function
  | Fail d     -> Fail d
  | Succ (g,d,i,s as m) ->
    if !trace then Printf.printf "i=%d g=[%s],d=%d,e=[%s],s=%d\n"
      i (String.concat "; " (List.map Ast.show g)) (Array.length d) (show (e s)) (List.length s);
    match m with
    |   [], d,  i, s -> Succ m
    |    g, d, -2, s -> (match pop m with Succ(g,d,i,s) when i > 0 -> step(Succ(g,d,-2,s))| m -> m)
    |    g, d, -1, s -> !runtime m
    | t::g, d,  i, s ->
      if i=0 || Array.length d = 4 then (pop m) else
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
        | None   -> Succ(       t::g, d, nx, s)
        | Some e -> Succ(gen_t t3::g, d,    -1, (t::g, e, l1, nx) :: s)
        end
      | (_,nx) -> Succ(t::g,d,nx,s)
and solve m =
  let m = match m with
    | [], _, _, _ -> pop m
    | _           -> Succ m
  in
  match step m with
  | (Succ ([],_,_,_) as m) -> m
  | Succ m -> solve m
  | m -> m
              
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

let () = runtime := (fun (g,d,i,s)->step (Succ(g, d, Vm_db.get_start d, s)))
