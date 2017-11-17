a(A,B) :- write(A),writeln(B).
a(A,B,C) :- write(A),write(B),writeln(C).
:- call(writeln,test),call(a(a),b),call(a(a,b),c),call(a,a,b,c),halt.
