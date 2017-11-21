%{
open Syntax
let xe = ""
%}
%token <string> ATOM
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
query:    | exp DOT                    { opconvert $1 }
sentence: | EOF                        { Atom "" }
          | exp DOT                    { opconvert $1 }
exp:      | exp1                       { $1 }
          | exp1 exp                   { Pred("",[$1;$2]) }
          | exp1 COMMA exp             { Pred("",[$1;Pred("",[Atom(",");$3])]) }
exp1:     | ATOM LPAREN exps RPAREN    { Pred($1, $3) }
          | ATOM                       { Atom($1) }
          | VAR                        { Var($1,1) }
          | NUMBER                     { Number($1) }
          | STR                        { Str($1) }
          | LBRACKET listbody RBRACKET { $2 }
          | LPAREN exp RPAREN          { Pred("",[$2;Atom""]) }
          | LBRACE exp RBRACE          { Pred("{}",[$2]) }
          | OP                         { Atom($1) }
          | BAR                        { Atom("|") }
exp2:     | exp1                       { $1 }
          | exp1 exp2                  { Pred("",[$1;$2]) }
exps:     | /* empty */                { [] }
          | exp2                       { [$1] }
          | exp2 COMMA exps            { $1::$3 }
listbody: | /* empty */                { Atom "[]" }
          | exp1                       { Pred("[|]",[$1;Atom"[]"]) }
          | exp1 COMMA listbody        { Pred("[|]",[$1;$3])}
          | exp1 BAR exp               { Pred("[|]",[$1;$3])}
