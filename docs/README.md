# GDIS Prolog 仕様

# ライブラリリファレンス

- [ライブラリリファレンス](library.md) を参照してください。

# BNF (version 0.0.4時点)

Lexical Syntax

```
upper       ::= ['A'-'Z'] | '\xce' ['\x91' - '\xa9']
lower       ::= ['a'-'z'] | '\xce' ['\xb1' - '\xbf'] | '\xcf' ['\x80' - '\x89']
digit       ::= ['0'-'9']
atom        ::= lower (lower|upper|digit|'_'|'`')*
VAR         ::= upper (lower|upper|digit|'_'|'`')*
nonendl     ::= [^'\n']*
number      ::= digit+ ('.' digit+)? 
str         ::= ([^ '"' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '"'])*
satom       ::= ([^ '\'' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '\''])*
op          ::= ";" | "," | "=" | "is" | "+" | "-" | "*" | "/" | "\\="
com         ::= [' ' '\t']* '(' [^ ')']* ')'
ln          ::= ('\r' '\n') | '\r' | '\n'
ln2         ::= [' ' '\t']* ln [' ' '\t']*
white space ::= [' ' '\t'] | ln
comment     ::= "%" nonendl
DOT         ::= ln2 ln2+ '.'? ln2* | "." ln2*
LINE        ::= '-' '-'+ com?
ATOM        ::= atom | "!" | "'" (satom as s) "'"
NUMBER      ::= number
STR         ::= '"' (str as s) '"'
```

Syntatical Syntax

```
query      ::= term DOT
seq        ::= DOT seq
             | sentence
             | sentence seq
sentence   ::= term DOT
             | "::=" term DOT
             | LINE term DOT
             | term LINE term DOT
             | term "::=" term DOT
term       ::= term1 ";" term
             | term1
term1      ::= exp1 "," term1
             | exp1 term1
             | exp1
exp1       ::= exp OP exp1
             | exp
exp        ::= ATOM "(" exps ")"
             | ATOM
             | var
             | NUMBER
             | STR
             | "[" listbody "]"
             | "(" term ")"
exps       ::= /* empty */
             | exp1
             | exp1 "," exps
listbody   ::= exps
             | exps "|" var_or_list
var_or_list::= VAR
             | "[" listbody "]"
```
