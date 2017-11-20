{
  open Parser
}
let upper = ['A'-'Z'] | '\xce' ['\x91' - '\xa9']
let lower = ['a'-'z'] | '\xce' ['\xb1' - '\xbf'] | '\xcf' ['\x80' - '\x89']
let digit = ['0'-'9']
let atom = lower (lower|upper|digit|'_'|'`')*
let var = upper (lower|upper|digit|'_'|'`')*
let nonendl = [^'\n']*
let number = digit+ ('.' digit+)? 
let str = ([^ '"' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '"'])*
let satom = ([^ '\'' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '\''])*

let op = ";" | "," | "=" | "is" | "+" | "-" | "*" | "/" | "\\="
let com = [' ' '\t']* '(' [^ ')']* ')'
let ln = ('\r' '\n') | '\r' | '\n'
rule token = parse
  | [' ' '\t']           { token lexbuf }
  | ln                   { token lexbuf }
  | ";"                  { SEMI }
  | ","                  { COMMA }
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | "["                  { LBRACKET }
  | "]"                  { RBRACKET }
  | "|"                  { OR }
  | "."                  { DOT }
  | "!"                  { ATOM("!") }
  | ":-"                 { IIF }
  | "\\"                 { PRE "\\" }
  | op     as s          { OP s }
  | atom   as s          { ATOM s }
  | var    as s          { VAR s }
  | number as s          { NUMBER (float_of_string s) }
  | '"' (str as s) '"'   { STR (Scanf.unescaped s) }
  | "'" (satom as s) "'" { ATOM (Scanf.unescaped s) }
  | "%" nonendl          { token lexbuf }
  | eof                  { EOF }
  | _                    { token lexbuf }
