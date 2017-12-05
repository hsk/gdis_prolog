a(A,B) :- write(A),writeln(B).
a(A,B,C) :- write(A),write(B),writeln(C).
:- call(writeln,test),call(a(a),b),call(a(a,b),c),call(a,a,b,c).
:- call((A=1;A=2,!)),A=2,writeln(1),!;writeln(2),!.
:- call((A=1)),A=2,writeln(1),!;writeln(2),!.

:- call(halt).
