sum(N, R) :- N=0,R=0.
sum(N, R) :- N=1,R=1.
:- sum(1,R),write(R),nl,halt.
