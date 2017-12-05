writeln2(A) :- write(A),nl,!.
name(test).
name(hoge).
name(fuga).
:- forall(name(A),writeln2(A)).
:- halt.
