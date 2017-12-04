assert(A) :- assertz(A).
nl :- write('\n').
writeln(A) :- write(A),nl.

append([],B,B).
append([A|AS],B,[A|B_]) :- append(AS,B,B_).

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

foldl(A,[],R,R).
foldl(A,[X|XS],R,R2) :- call(A,X,R,R1),foldl(A,XS,R1,R2).

forall(A,B) :- call(A),call(B),fail;true.

findall(A,B,R) :- findall1(A,B,R1),reverse(R1,R).
findall1(A,B,R) :- asserta(findall1([])),
  forall(B,findall2(A)),
  findall1(R),
  retract(findall1(Z)).
findall2(A):-findall1(L),retract(findall1(L)),asserta(findall1([A|L])).

discontiguous(A).

expand_term(T,T_) :- goal_expansion(T,T1),!,expand_term(T1,T_),!
  ; T = (T1,T2), !,expand_term(T1,T1_),expand_term(T2,T2_),T_=(T1_,T2_),!
  ; T = (T1;T2), !,expand_term(T1,T1_),expand_term(T2,T2_),T_=(T1_;T2_),!
  ; T = (T1->T2),!,expand_term(T1,T1_),expand_term(T2,T2_),T_=(T1_->T2_),!
  ; T_=T,!.
macro_run(T) :-
  (term_expansion(T,T1);T=T1),
  (T1=(:- T3),!,expand_term(T3,T3_),!,call(T3_)
  ;T1=(A1:-A2),expand_term(A2,A2_),assertz(A1:-A2)
  ;assertz(T1)
  ;writeln(error:T1)).

consult_loop([]).
consult_loop([P|PS]) :- opconvert(P,P_), (macro_run(P_);writeln(false)),!,consult_loop(PS).
consult(A) :- read(A,B),!,consult_loop(B).
