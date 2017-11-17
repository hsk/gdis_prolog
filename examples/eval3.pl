% Syntax

i(I) :- integer(I).
e(A) :- i(A).
e(E1+E2) :- e(E1),e(E2).
e(E1*E2) :- e(E1),e(E2).

% Typing

integer(I)
-------------------------------- (T-Int)
typing(I,int)

typing(E1,int)    typing(E2,int)
-------------------------------- (T-Add)
typing(E1+E2,int)

typing(E1,int)    typing(E2,int)
-------------------------------- (T-Mul)
typing(E1*E2,int)

% Evaluation

integer(A)
---------- (E-Int)
eval(A,A)

eval(A,R1) eval(B,R2) R is R1 + R2
---------------------------------- (E-Add)
eval(A+B,R)

eval(A,R1) eval(B,R2) R is R1 * R2
---------------------------------- (E-Add)
eval(A*B,R)

run(E,R) :- e(E), typing(E,int), eval(E,R).

:- run(1+2*3,R),write(R),nl,!,halt.
