sum(0, 0) :- !.
sum(1, 1).
sum(N, R) :- N1 is N - 1,!, sum(N1, R1),!, R is N + R1,!.
:- sum(10,R).
