%{
open Syntax
let prefixs  = opn 10001 ["fx";"fy"]
let postfixs = opn 10001 ["xf";"yf"]
let infixrs  = opn 10001 ["xfy";"xfx"]
let infixs   = opn 10001 ["yfx"]
let rec opconvert e =
	fst (opconvert_pre 10000 e)
and opconvert_pre p = function
	| Pred("",[Atom(op);y]) when prefixs op <= p ->
		let (t,ts) = opconvert_pre (prefixs op) y in
		(Pred(op,[t]),ts)
	| Pred("",[Pred("",xs);y]) ->
		let (t,ts) = opconvert_pre 10000 (Pred("",xs)) in
		opconvert_post p t y
	| Pred("",[x;y]) -> opconvert_post p (opconvert x) y
	| Pred(a,s) -> (Pred(a,List.map opconvert s),Atom"")
	| e -> (e, Atom "")
and opconvert_post p t = function
	| Pred("",[Atom(op);y]) when infixs op < p ->
		let (t2,ts2) = opconvert_pre (infixs op) y in
		opconvert_post p (Pred(op,[t;t2])) ts2
	| Pred("",[Atom(op);y]) when infixrs op <= p ->
		let (t2,ts2) = opconvert_pre (infixrs op) y in
		opconvert_post p (Pred(op,[t;t2])) ts2
	| Pred("",[Atom(op);y]) when postfixs op <= p ->
		opconvert_post p (Pred(op,[t])) y
	| tokens -> (t,tokens)
%}
%token <string> ATOM %token <string> ATOM_LPAREN
%token <float> NUMBER
%token <string> STR
%token <string> VAR
%token <string> OP
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COMMA BAR DOT
%token EOF
%token <string> COMMENT
%start query
%type <Syntax.t> query
%start sentence
%type <Syntax.t> sentence
%%
query:    | body3 DOT                   { opconvert $1 }
sentence: | EOF                         { Atom "" }
          | body3 DOT                   { opconvert $1 }
body3:    | op3                         { $1 }
body2:    | /* empty */                 { [] }
          | op2                         { [$1] }
          | op2 COMMA body2             { $1::$3 }
body1:    | /* empty */                 { Atom "[]" }
          | op1                         { Pred("[|]",[$1;Atom"[]"]) }
          | op1 COMMA body1             { Pred("[|]",[$1;$3])}
          | op1 BAR body3               { Pred("[|]",[$1;$3])}
op3:      | exp3                        { $1 }
          | exp3 op3                    { Pred("",[$1;$2]) }
op2:      | exp2                        { $1 }
          | exp2 op2                    { Pred("",[$1;$2]) }
op1:      | exp1                        { $1 }
          | exp1 op1                    { Pred("",[$1;$2]) }
exp3:     | exp2                        { $1 }
          | COMMA                       { Atom(",") }
exp2:     | exp1                        { $1 }
          | BAR                         { Atom("|") }
exp1:     | ATOM_LPAREN body2 RPAREN    { Pred($1, $2) }
          | ATOM                        { Atom($1) }
          | VAR                         { Var($1,0) }
          | NUMBER                      { Number($1) }
          | STR                         { Str($1) }
          | LBRACKET body1 RBRACKET     { $2 }
          | LPAREN body3 RPAREN         { Pred("",[$2;Atom""]) }
          | LBRACE body3 RBRACE         { Pred("{}",[$2]) }
          | OP                          { Atom($1) }
