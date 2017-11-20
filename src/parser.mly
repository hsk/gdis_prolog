%{
open Syntax
let rec list args tail = match args with
  | []    -> tail
  | x::xs -> Pred(".", [x; list xs tail])
%}
%token <float> NUMBER %token <string> ATOM STR VAR OP PRE
%token LPAREN RPAREN LBRACKET RBRACKET DOT OR SEMI COMMA IIF EOF
%right IIF %right COMMA %right OP
%start sentence %type <Syntax.t> sentence
%start query %type <Syntax.t> query
%%
query:       | term DOT                   { $1 }
sentence:    | term DOT                   { Pred(":-", [$1; Atom "true"]) }
             | IIF term DOT               { Pred(":-", [$2]) }
             | term IIF term DOT          { Pred(":-", [$1; $3]) }
             | EOF                        { Atom ("false") }
term:        | term1 SEMI term            { Pred(";", [$1; $3]) }
             | term1                      { $1 }
term1:       | exp1 COMMA term1           { Pred(",", [$1; $3]) }
             | exp1 term1                 { Pred(",", [$1; $2]) }
             | exp1                       { $1 }
exp1:        | exp OP exp1                { Pred($2, [$1; $3]) }
             | exp                        { $1 }
exp:         | ATOM LPAREN exps RPAREN    { Pred($1, $3) }
             | ATOM                       { Atom $1 }
             | VAR                        { Var($1, 0) }
             | NUMBER                     { Number $1 }
             | STR                        { Str $1 }
             | LBRACKET listbody RBRACKET { $2 }
             | LPAREN term RPAREN         { $2 }
             | PRE exp                    { Pred($1, [$2]) }
exps:        | /* empty */                { [] }
             | exp1                       { [$1] }
             | exp1 COMMA exps            { $1::$3 }
listbody:    | exps                       { list $1 (Atom "[]") }
             | exps OR exp                { list $1 $3 }
