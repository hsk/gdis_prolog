:- consult('lib/rtg.pl').

integer     ::= syntax.
i           ::= integer.
/*
list(A)     ::= [] | [A|list(A)].
e           ::= i | x | list(e) | escape(e | e) | e+e.
*/
:- integer(1).
:- i(1).
/*
:- e([1,2,3+2+3]).
:- e([1,2,3+2+a]),writeln(error);true.
:- e([1,2,3+2+x]).
:- e(1|2).
*/
:- halt.
