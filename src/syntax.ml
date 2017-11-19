type v = string * int
type t =
  | Atom of string
  | Number of float
  | Str of string
  | Pred of string * t list
  | Var of v

let rec show = function
  | Atom(n)           -> n
	| Number(v)         -> let s = string_of_float v in
												let l = String.length s in
												if String.sub s (l-1) 1 = "." then String.sub s 0 (l-1) else s
  | Str(v)            -> v
  | Pred(".", _) as t -> Printf.sprintf "[%s]" (show_list t)
  | Pred(n, xs)       -> Printf.sprintf "%s(%s)" n (String.concat ", " (List.map show xs))
  | Var(n, l)         -> Printf.sprintf "%s_%d" n l

and show_list = function
  | Pred(".", [t; Atom("[]")])         -> show t
  | Pred(".", [t;(Pred(".", _) as u)]) -> show t ^ "," ^ show_list u
  | Pred(".", [t;u])                   -> show t ^ "|" ^ show u
  | t                                  -> show t
