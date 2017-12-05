writeln2(A) :- write(A),nl,!.
name(test).
name(hoge).
:- forall(name(A),writeln2(A)).
:- halt.
