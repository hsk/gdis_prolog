assert(A) :- assertz(A).
nl :- write('\n').
writeln(A) :- write(A),nl.

member(A,[A|B]).
member(A,[C|B]) :- member(A,B).

maplist(A,[]).
maplist(A,[X|XS]) :- call(A,X),maplist(A,XS).

maplist(A,[],[]).
maplist(A,[X|XS],[R|RS]) :- call(A,X,R),maplist(A,XS,RS).

maplist(A,[],[],[]).
maplist(A,[X|XS],[R|RS],[S|SS]) :- call(A,X,R,S),maplist(A,XS,RS,SS).

reverse(A,R) :- reverse1(A,[],R).
reverse1([],R,R).
reverse1([A|AS],B,R) :- reverse1(AS,[A|B],R).

foldl(A,R,[],R).
foldl(A,R,[X|XS],R2) :- call(A,X,R,R1),foldl(A,R1,XS,R2).

forall(A,B) :- call(A),call(B),fail;true.

findall(A,B,R) :- findall1(A,B,R1),reverse(R1,R).
findall1(A,B,R) :- asserta(findall1([])),
  forall(B,findall2(A)),
  findall1(R),
  retract(findall1(Z)).
findall2(A):-findall1(L),retract(findall1(L)),asserta(findall1([A|L])).

discontiguous(A) :- writeln(A),true.
