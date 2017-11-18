nl :- write("\n").
writeln(A) :- write(A),nl.

member(A,[A|B]).
member(A,[C|B]) :- member(A,B).

maplist(A,[]).
maplist(A,[X|XS]) :- call(A,X),maplist(A,XS).

maplist(A,[],[]).
maplist(A,[X|XS],[R|RS]) :- call(A,X,R),maplist(A,XS,RS).

maplist(A,[],[],[]).
maplist(A,[X|XS],[R|RS],[S|SS]) :- call(A,X,R,S),maplist(A,XS,RS,SS).

foldl(A,R,[],R).
foldl(A,R,[X|XS],R2) :- call(A,X,R,R1),foldl(A,R1,XS,R2).
