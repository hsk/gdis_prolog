eval(A,A) :- integer(A),!.
eval(A+B,R) :- eval(A,R1),eval(B,R2),R is R1 + R2.
eval(A*B,R) :- eval(A,R1),eval(B,R2),R is R1 * R2.

:- eval(1+2,R),write(R),nl,!,halt.

