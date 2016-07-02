s(N,R) :- !, N=0,R=0.
s(N,R) :- N=R.

:- s(1,R),write(R),nl,halt.
