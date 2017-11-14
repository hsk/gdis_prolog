eval(int(A),A) :- !.
eval(add(A,B),R) :- eval(A,R1),eval(B,R2),R is R1 + R2.
eval(mul(A,B),R) :- eval(A,R1),eval(B,R2),R is R1 * R2.

:- eval(add(int(1),int(2)),R),write(R),nl,!,halt.

