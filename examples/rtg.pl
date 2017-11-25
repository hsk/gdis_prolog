:- consult('lib/rtg.pl').

integer     ::= syntax.
list(A)     ::= [] | [A|list(A)].
i           ::= integer.
e           ::= i | x | list(e) | escape(e | e) | e+e.

:- i(1).
:- e([1,2,3+2+3]).
:- e([1,2,3+2+a]),writeln(error);true.
:- e([1,2,3+2+x]).
:- e(1|2).
:- halt.
