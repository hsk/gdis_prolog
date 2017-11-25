:- var(A).
:- A=B,var(A).

test(A) :- var(A).

:- test(A).
:- A=1,test(A),writeln(error);true.
:- halt.
