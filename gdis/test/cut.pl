s(N,R) :- N=1,R=0.
s(N,R) :- N=2,R=2.
s(N,R) :- writeln(fail),N=2,!,fail.
s(N,R) :- N=1,writeln(fail2),!,fail.
s(N,R) :- writeln(fail3),!,fail.

:- s(1,R),writeln(R),writeln(r=R),R\=0,halt;writeln(ng),halt.
