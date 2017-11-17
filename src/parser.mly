%{
open Syntax
let rec list args tail = match args with
  | []    -> tail
  | x::xs -> Pred(".", [x; list xs tail])
%}
%token <float> NUMBER %token <string> ATOM STR VAR OP
%token LPAREN RPAREN LBRACKET RBRACKET DOT OR SEMI COMMA LINE IIF EOF
%right IIF %right COMMA %right OP
%start seq %type <Syntax.t list> seq
%start query %type <Syntax.t> query
%%
query:       | term DOT                   { $1 }
seq:         | sentence                   { [$1] }
             | sentence seq               { $1::$2 }
sentence:    | term DOT                   { Pred(":-", [$1; Atom "nop"]) }
             | IIF term DOT               { Pred(":-", [$2]) }
             | LINE term DOT              { Pred(":-", [$2; Atom "nop"]) }
             | term LINE term DOT         { Pred(":-", [$3; $1]) }
             | term IIF term DOT          { Pred(":-", [$1; $3]) }
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
exps:        |                            { [] }
             | exp1                       { [$1] }
             | exp1 COMMA exps            { $1::$3 }
listbody:    | exps                       { list $1 (Atom "[]") }
             | exps OR var_or_list        { list $1 $3 }
var_or_list: | VAR                        { Var($1, 0) }
             | LBRACKET listbody RBRACKET { $2 }
