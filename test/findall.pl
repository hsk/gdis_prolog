name(test).
name(hoge).
name(fuga).
copy_term(A,B) :- A=B.
findall1(A,B,R) :- writeln(aaaaaa),asserta(findall1([])),writeln(bbbb),
  forall(B,writeln(A)),
  writeln(kkkkkkkkk),!,
  forall(B,(findall1(L),retract(findall1(L)),write([A|L]),asserta(findall1([A|L])),writeln(ok1))),
  writeln(ok),!,
  findall1(R),
  writeln(r=R),
  retract(findall1(_)).

:- asserta(a(1)),asserta(a(2)),forall(a(R),writeln(R)),retract(a(Z)),forall(a(R),writeln(R)).
:- writeln(aaa).
:- forall(a(R),writeln(R)).
:- forall(name(A),writeln(A)).
:- writeln(cccccc),findall1(A,name(A),R),writeln(R),!,maplist(writeln,R).
:- halt.
