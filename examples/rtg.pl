:- consult('lib/rtg.pl').

integer ::= syntax.
i       ::= integer.
list(A) ::= [] | [A|list(A)].
e       ::= i | x | list(e) | escape(e | e) | (e+e).

:- integer(1).
:- i(1).
:- e(1).
:- e(1+2).
:- e(x).
:- e([1,2,3]).
:- e([1,2,a]+2),writeln(error);true.
:- e([1,2,x]+1).
:- e(1|2).

:- halt.
