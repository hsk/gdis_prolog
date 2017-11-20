name(test).
name(hoge).
name(fuga).
forall1(A,B) :- call(A),call(B),fail;true.

findall2(A):-findall1(L),retract(findall1(L)),asserta(findall1([A|L])).

findall1(A,B,R) :- asserta(findall1([])),
  forall1(B,findall2(A)),
  findall1(R),
  retract(findall1(Z)).

:- findall1(A,name(A),R),writeln(R).
:- forall1(findall1(A),writeln(A)).
:- halt.
