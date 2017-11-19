name(test).
name(hoge).
name(fuga).
:- forall(name(A),writeln(A)).
:- halt.
