name(test).
name(hoge).
name(fuga).

:- findall(A,name(A),R),writeln(R).
:- forall(findall1(A),writeln(A)).
:- halt.
