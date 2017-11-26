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
query:    | expr3 DOT                { opconvert $1 }
sentence: | EOF                      { Atom "" }
          | expr3 DOT                { opconvert $1 }
expr3:    | term3                    { $1 }
expr2:    | /* empty */              { [] }
          | term2                    { [$1] }
          | term2 COMMA expr2        { $1::$3 }
expr1:    | /* empty */              { Atom "[]" }
          | term1                    { Pred("[|]",[$1;Atom"[]"]) }
          | term1 COMMA expr1        { Pred("[|]",[$1;$3]) }
          | term1 BAR expr3          { Pred("[|]",[$1;$3]) }
term3:    | fact3                    { $1 }
          | fact3 term3              { Pred("",[$1;$2]) }
term2:    | fact2                    { $1 }
          | fact2 term2              { Pred("",[$1;$2]) }
term1:    | fact1                    { $1 }
          | fact1 term1              { Pred("",[$1;$2]) }
fact3:    | fact2                    { $1 }
          | COMMA                    { Atom(",") }
fact2:    | fact1                    { $1 }
          | BAR                      { Atom("|") }
fact1:    | ATOM_LPAREN expr2 RPAREN { Pred($1, $2) }
          | ATOM                     { Atom($1) }
          | VAR                      { Var($1,0) }
          | NUMBER                   { Number($1) }
          | STR                      { Str($1) }
          | LBRACKET expr1 RBRACKET  { $2 }
          | LPAREN expr3 RPAREN      { Pred("",[$2;Atom""]) }
          | LBRACE expr3 RBRACE      { Pred("{}",[$2]) }
          | OP                       { Atom($1) }
