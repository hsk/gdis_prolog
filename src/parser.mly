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
query:    | exp DOT                     { opconvert $1 }
sentence: | EOF                         { Atom "" }
          | exp DOT                     { opconvert $1 }
exp:      | exp3s                       { $1 }
exp3s:    | exp3                        { $1 }
          | exp3 exp3s                  { Pred("",[$1;$2]) }
exp2s:    | exp2                        { $1 }
          | exp2 exp2s                  { Pred("",[$1;$2]) }
exp1s:    | exp1                        { $1 }
          | exp1 exp1s                  { Pred("",[$1;$2]) }
exp3:     | exp2                        { $1 }
          | COMMA                       { Atom(",") }
exp2:     | exp1                        { $1 }
          | BAR                         { Atom("|") }
exp1:     | ATOM_LPAREN exp2body RPAREN { Pred($1, $2) }
          | ATOM                        { Atom($1) }
          | VAR                         { Var($1,0) }
          | NUMBER                      { Number($1) }
          | STR                         { Str($1) }
          | LBRACKET exp1body RBRACKET  { $2 }
          | LPAREN exp RPAREN           { Pred("",[$2;Atom""]) }
          | LBRACE exp RBRACE           { Pred("{}",[$2]) }
          | OP                          { Atom($1) }
exp2body: | /* empty */                 { [] }
          | exp2s                       { [$1] }
          | exp2s COMMA exp2body        { $1::$3 }
exp1body: | /* empty */                 { Atom "[]" }
          | exp1s                       { Pred("[|]",[$1;Atom"[]"]) }
          | exp1s COMMA exp1body        { Pred("[|]",[$1;$3])}
          | exp1s BAR exp               { Pred("[|]",[$1;$3])}
