open Syntax

type db = (Syntax.t * int) array

let empty () = Array.copy [|
  Atom"freelist",0;
  Atom"start",3;
  Atom"end",2;
|]

let dump db =
  let i = ref 0 in
  Array.iter(fun (p,n) ->
    Printf.printf "%d : %s %d\n" !i (show p) n;
    i := !i + 1
  ) db

let get_free (db:db):int = snd db.(0)
let get_start db = snd db.(1)
let get_end db = snd db.(2)
let new_free (db:db) p n : (int * db) =
  let free = get_free db in
  if free = 0 then (Array.length db,Array.append db [|p,n|])
  else (
    failwith "error use free datas";
    (*db.(0) <- (fst db.(0), snd db.(free));
    db.(free) <- (p,n);
    (free,(db:db))*)
  )

let assert1 (db:db) p =
  let endp = get_end db in
  let (newp,db) = new_free db p 0 in
  db.(endp) <- (fst db.(endp),newp);
  db.(2) <- (fst db.(2),newp);
  db

let remove db n =
  db.(n) <- (Number (float_of_int (snd db.(0))), snd db.(n));
  db.(0) <- (Number (float_of_int n), snd db.(0));
  db

let retract db (f:Syntax.t->bool) =
  let rec loop i db =
    if i = 0 then db else
    let (t,n) = db.(i) in
    if f t then remove db i else loop n db
  in
  loop (get_start db) db

let asserta db p =
  let startp = get_start db in
  let (newp,db) = new_free db p startp in
  db.(1) <- (fst db.(1),newp);
  db

let retractz db f =
  let rec loop i p =
    if i = 0 then p else
    let (t,n) = db.(i) in
    loop n (if f t then i else p)
  in
  let p = loop (get_start db) 0 in
  if p = 0 then db else remove db p

let retractall db f =
  let rec loop i db =
    if i = 0 then db else
    let (t,n) = db.(i) in
    loop n (if f t then remove db i else db)
  in
  loop (get_start db) db
