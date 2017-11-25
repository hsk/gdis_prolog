{
  open Parser
}
let upper = ['A'-'Z'] | '\xce' ['\x91' - '\xa9']
let lower = ['a'-'z'] | '\xce' ['\xb1' - '\xbf'] | '\xcf' ['\x80' - '\x89']
let digit = ['0'-'9']
let atom = lower (lower|upper|digit|'_'|'`')*
let var = ('_'|upper) (lower|upper|digit|'_'|'`')*
let nonendl = [^'\n']*
let number = digit+ ('.' digit+)? 
let str = ([^ '"' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '"'])*
let satom = ([^ '\'' '\\'] | '\\' ['\\' '/' 'b' 'f' 'n' 'r' 't' '\''])*

let op = "=" | "+" | "-" | "*" | "/" | ":" | ">" | "<" | "?" | "\\" | "@" | "^"
let com = [' ' '\t']* '(' [^ ')']* ')'
let ln = ('\r' '\n') | '\r' | '\n'
let ln2 = [' ' '\t']* ln [' ' '\t']*
rule token = parse
  | ([' ' '\t' '\n']* "%" nonendl) { token lexbuf }
  | ([' ' '\t' '\n']* "/*")        { comment lexbuf; token lexbuf }
  | [' ' '\t' '\n']      { token lexbuf }
  | "=.."                { OP("=..") }
  | "|"                  { BAR }
  | ";"                  { OP(";") }
  | ","                  { COMMA }
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | "["                  { LBRACKET }
  | "]"                  { RBRACKET }
  | "{"                  { LBRACE }
  | "}"                  { RBRACE }
  | "."                  { DOT }
  | "!"                  { OP("!") }
  | "*/"                 { failwith "found */ error." }
  | op+     as s         { OP s }
  | atom   as s '('          { ATOM_LPAREN s }
  | "'" (satom as s) "'" '(' { ATOM_LPAREN (Scanf.unescaped s) }
  | atom   as s          { ATOM s }
  | "'" (satom as s) "'" { ATOM (Scanf.unescaped s) }
  | var    as s          { VAR s }
  | number as s          { NUMBER (float_of_string s) }
  | '"' (str as s) '"'   { STR (Scanf.unescaped s) }
  | eof                  { EOF }
  | _                    { token lexbuf }
and comment = parse
  | "*/" { () }
  | "/*" { comment lexbuf; comment lexbuf }
  | eof { Format.eprintf "warning: unterminated comment@."; () }
  | _ { comment lexbuf }
