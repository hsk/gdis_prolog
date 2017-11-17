w(A,B) :- write(A),write(':'),writeln(B).
w(A,B,C) :- write(A),write(':'),write(B),write(':'),writeln(C).
:- maplist(writeln,[1,2,3]),!,maplist(w,[a,b,c],[1,2,3]),!,maplist(w,[a,b,c],[1,2,3],[d,e,f]),halt.
