open Syntax
(*
DBは述語と次のアドレスが入った配列です。
ただし、０番地にはフリーリスト、１番目はリストの開始番地、２番めには終了番地が書かれています。

*)
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

(* 開始アドレス取得 *)
let get_free (db:db):int = snd db.(0)

(* 開始アドレス取得 *)
let get_start db = snd db.(1)

(* 終了アドレス取得 *)
let get_end db = snd db.(2)

(* DBに追加してアドレスを返します。ただし、リンク情報は追加されません。*)
let new_free (db:db) p n : (int * db) =
	let free = get_free db in
	if free = 0 then (Array.length db,Array.append db [|p,n|])
	else ((* フリーデータを１つ取り出し繋ぎ変えます。 *)
		db.(0) <- (fst db.(0), snd db.(free));
		db.(free) <- (p,n);
		(free,(db:db))
	)

(*
dbの後ろに追加
フリーリストがあればフリーリストからデータを取り出し終了位置の後ろにつなげます。終了位置に設定します。
フリーリストがなければ、一番下に追加します。
*)
let assert1 (db:db) p =
	(*Printf.printf "\n-----\nassert1 %s\n" (show p);
	dump db;*)
	let endp = get_end db in
	let (newp,db) = new_free db p 0 in
	db.(endp) <- (fst db.(endp),newp); (* リンクにつなぎ *)
	db.(2) <- (fst db.(2),newp); (* 終了位置を保存 *)
	(*Printf.printf "\n-----\n";
	dump db;*)
	db

(* 指定インデックスを削除 *)
let remove db i n =
	db.(i) <- (fst db.(i), snd db.(n));
	db.(n) <- (Atom "free",snd db.(0));
	db.(0) <- (fst db.(0), n);
	db.(2) <- (fst db.(2), i);
	db
(* dbの手前から１つ削除 *)
let retract db (f:Syntax.t->bool) =
	(*dump db;*)
	let rec loop i db =
		if i = 0 || Array.length db <= i then db else
		match db.(i) with
		| (_,0) -> db
		| (_,n) when f (fst db.(n)) -> remove db i n
		| (_,n) -> loop n db
	in
	let db = loop (get_start db) db in
	(*dump db;*)
	db
	(* fがtrueを返すデータをみつけて１つ消す*)

(*dbの手前に追加
フリーリストがあればフリーリストからデータを取り出し開始アドレスに追加します。
フリーリストがなければ終了位置に追加します。
*)
let asserta db p =
	let startp = get_start db in
	let (newp,db) = new_free db p startp in
	db.(1) <- (fst db.(1),newp); (* 開始位置を保存 *)
	db

(* dbの後ろからから１つ削除 *)
let retractz db f =
	(* fがtrueを返すデータをみつけて１つ消す*)
	assert false

(* 全部消す *)
let retractall db f =
	assert false

(* 一つ取り出す *)
let get db i = assert false
