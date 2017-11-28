#!/usr/bin/env swipl --toplevel=halt --stand_alone=true -q
:- initialization(main).
:- style_check(-singleton).

% syntax

syntax_show(atom(N),       N).
syntax_show(num(V)    , R) :- atom_number(R,V).
syntax_show(str(V)      , V).
syntax_show(pred('.', Ls), R) :- show_list(pred('.', Ls), Lt), format(atom(R), "[~s]", Lt). 
syntax_show(pred(N, Xs)  , R) :- maplist(syntax_show, Xs, Xs1),
                                 atomics_to_string(Xs1,", ", Ls),
                                 format(atom(R), "~s(~s)", [N, Ls]).
syntax_show(var((N, L))  , R) :- format(atom(R), "~s_~d", [N, L]).

show_list(pred('.', [T,  atom('[]')]), R) :- !,syntax_show(T, R).
show_list(pred('.', [T,pred('.', L)]), R) :- !,syntax_show(T, R1), show_list(pred('.', L), R2), format(atom(R), "~s, ~s", [R1, R2]).
show_list(pred('.', [T,U])           , R) :- !,syntax_show(T,R1),syntax_show(U,R2), format(atom(R), "~s|~s", [R1, R2]).
show_list(T                          , R) :- !,syntax_show(T, R).

% reader

cnv_find(K=T,[K=T1|_]) :- T==T1.
cnv_find(K=T,[_=T1|Env]) :- T\==T1,cnv_find(K=T,Env).

cnv(Env,T,   atom(T)) :-     atom(T), !.
cnv(Env,T,var((K,0))) :-      var(T), !, cnv_find(K=T,Env),!.
cnv(Env,T,    num(T)) :-   number(T), !.
cnv(Env,T,    str(T)) :-   string(T), !.
cnv(Env,T,         R) :-  is_list(T), !, cnv_list(Env,T,R),!.
cnv(Env,T,pred(N,Ls)) :- T =..[N|Xs], !, maplist(cnv(Env),Xs,Ls),!.

cnv_list(Env,     [],atom('[]')).
cnv_list(Env,[T1|T2],pred('.',[R1,R2])) :- cnv(Env,T1,R1),cnv_list(Env,T2,R2).

cnv2(Env,T, R) :- cnv(Env, T, R1), (R1=pred(':-',_), R=R1; R=pred(':-',[R1,atom('nop')])).

parse(S,R) :- term_string(T,S,[variable_names(Env)]),!,cnv(Env,T,R),!.

% prolog

env_show_test_(((N,L),_)) :- L =< 1.
env_show_add_(E, ((N,_),T),R) :- deref(E,T,T2), syntax_show(T2,T3),format(atom(R),"~s=~s",[N,T3]).
env_show(E,R) :-
  include(env_show_test_,E,E2),
  maplist(env_show_add_(E),E2,E3),
  atomic_list_concat(E3,', ',R).

deref(E,pred(N, Ts), R) :- maplist(deref(E), Ts, Ts2), R=pred(N, Ts2).
deref(E,var(V),      R) :- member((V,V2),E), deref(E, V2,R).
deref(E,T,           T).

e([],[]).
e([(_,E,_,_)|_],E).

el1([],[],1).
el1([(_,E,L,_)|_],E,L1) :- L1 is L + 1.

pop((_, D, _, [(G, _,_, I)|S]), succ(G, D, I, S)).
pop((_, D, _,             []), fail(D)).

find(E,K,V) :- member((K,V),E).

bind(T, V, T2, E, E2) :-
  find(E, V, T3),!,
  (T3  = var(V3),!, (T\=T3, bind(T,V3,T2,E,E2),!)
  ;!,(T2=T3,E2=E,!;unify((T3,T2),E,E2),!)
  )
  ; E2=[(V,T2)|E].

zip([], [], []).
zip([X|Xs], [Y|Ys], [(X,Y)|Zs]) :- zip(Xs,Ys,Zs).

unify((         T,          T2), E,  E2) :- T2=var(V2),!, bind(T2, V2, T , E, E2).
unify((         T,          T2), E,  E2) :- T =var(V ),!, bind(T , V , T2, E, E2).
unify((pred(X, G), pred(X, G2)), E,  E2) :- !,zip(G, G2, GG), foldl(unify, GG, E, E2)/*,format('ok ~p~n',[pred(X,G)])*/.
unify((         T,           T), E,   E).

uni(M,E,T,T2,M2) :-
	unify((T,T2),E,E2), M=([_|G],D,_,[(Sg, _,L, I)|S]), M2=succ(G, D, -1,[(Sg,E2,L,I)|S])
  ; pop(M,M2).

eval(E, num(I), I).
eval(E, var(V), I) :- find(V,E,V2), e(V2,E2), eval(E, E2, I).
eval(E, pred('+', [T, T2]), I3) :- eval(E, T, I), eval(E, T2, I2), I3 is I + I2.
eval(E, pred('*', [T, T2]), I3) :- eval(E, T, I), eval(E, T2, I2), I3 is I * I2.
eval(E, pred('-', [T, T2]), I3) :- eval(E, T, I), eval(E, T2, I2), I3 is I - I2.
eval(E, pred('/', [T, T2]), I3) :- eval(E, T, I), eval(E, T2, I2), I3 is I div I2.

write1(E, T) :- deref(E,T,T2), syntax_show(T2, X), write(X).

assert1(pred(':-', [T]),D, R) :- process(D, T, R).
assert1(T              ,D, R) :- append(D, [T], R).

read_stream_to_terms(Stream, RC, REnv) :-
  read_term(Stream, C, [variable_names(Env)]),!,
  (C=end_of_file,
    RC=[], REnv=[],!
  ; read_stream_to_terms(Stream, RC1, REnv1),
    RC=[C|RC1], append(Env,REnv1,REnv),!
  ).

consult1(T,D,D2) :-
  syntax_show(T, Filename),
  (flag(trace,Tr,Tr),Tr=1,write("Loading "),write(Filename),nl;!),!,
  setup_call_cleanup(
    open(Filename, read, In),
    read_stream_to_terms(In, Terms, Env),
    close(In)
  ),
  maplist(cnv2(Env),Terms,Terms2),!,
  foldl(assert1, Terms2, D, D2),!.

step(succ(G,D,I,S), R2) :-
  (flag(trace,Tr,Tr),Tr=1,format("i=~d G=~p S=~p~n",[I,G,S]);!),!,
  step1((G,D,I,S), G,D,I,S, R2).
step(R, R).

step1(M,                     [ ], D,  I, S, R) :- R=succ([], D,  I, S),!.
step1(M,                      G , D, -2, S, R) :- R=fail(D).
step1(M,[atom('halt')        |G], D, -1, S, R) :- halt.
step1(M,[atom('nop')         |G], D, -1, S, R) :- step(succ(G, D, -1, S), R).
step1(M,[atom('!')           |G], D, -1, [(G2,E,L,_)|S], R) :- step(succ(G, D, -1, [(G2, E,L, -2)]), R).
step1(M,[pred(',',  [U,V])   |G], D, -1, S, R) :- step(succ([U,V|G], D, -1, S), R).
step1(M,[pred(';',  [U,V])   |G], D, -1, S, R) :- el1(S, E, L1), step(succ([U|G], D, -1, [([V|G], E,L1, -1)|S]), R).
step1(M,[pred('=',  [U,V])   |G], D, -1, S, R) :- e(S, E), uni(M,E,U,V,R1),step(R1, R).
step1(M,[pred('is', [U,V])   |G], D, -1, S, R) :- e(S, E), deref(E, V, V2), eval(E, V2, N), !, uni(M,E,U,num(N),R1), step(R1,R).
step1(M,[pred('assert',  [T])|_], D, -1, S, R) :- e(S, E), deref(E, T, T2), assert1(T2, D, D2), step(succ(G, D2, I, S), R).
step1(M,[pred('write',   [T])|G], D, -1, S, R) :- e(S, E), write1(E, T), step(succ(G, D, -1, S), R).
step1(M,[pred('consult', [T])|_], D, -1, S, R) :- e(S, E), deref(E, T, T2), consult1(T2, D, D2), step(succ(G, D2, I, S),R).
step1(M,                      G , D, -1, S, R) :- step(succ(G, D, 0, S), R).
step1(M,                   [T|G], D,  I, S, R) :- nth0(I, D, pred(':-', [T2, T3])),!, el1(S, E, L1), gen_t(L1, T2, T2d), I1 is I + 1,
                                                  ( unify((T, T2d), E, E2), gen_t(L1,T3, T3d),
                                                    step(succ([T3d|G], D, -1, [([T|G], E2,L1, I1)|S]),R)
                                                  ; step(succ([T| G], D, I1, S), R)).
step1(M,                      G , D,  I, S, R) :- pop(M,M2), step(M2,R).

gen_t(L, pred(N,Ts), pred(N,Ts2)) :- maplist(gen_t(L), Ts, Ts2).
gen_t(L, var((N,_)), var((N,L))).
gen_t(L, T, T).

solve(M, R2) :- ([], _, _, _) = M, pop(M, R), step(R, R2).
solve(M, R2) :- ( G, D, I, S) = M, step(succ(G,D,I,S), R2).

read_line(P, A) :- prompt1(P), read_line_to_codes(user_input,R),atom_codes(A,R).

prove(M,D2) :- solve(M, R),
  ( R = fail(D2)
  ; R = succ(G, D, I, S),
    e(S,V), env_show(V, X), write(X),nl,
    ((S=[]; I= -2), write("Yes.\n"), D2=D
    ; (read_line("More y/n", "y"), prove((G, D, I, S), D2)
      ; D2=D
      )
    )
  ).

process(D, T, D2) :- prove(([T], D, -1, [([],[],1,-2)]), D2).

% main

welcome("Beautiful Japanese Prolog Interpreter").

help :- maplist(format('~s\t~s\n'),[["e","exit"], ["l","list"],["h","help"]]).

syntax_print(T) :- syntax_show(T,R),write(R),nl.

repl(D) :-
    read_line("? ", Y),!,
    (Y='e'
    ;Y='l', maplist(syntax_print, D),!, repl(D)
    ;Y='h', help,!, repl(D)
    ;Y='t', flag(trace,Trace,1-Trace),write('Tracing '),
            (Trace=0,write('on');write('off')),nl,!,repl(D)
    ;         parse(Y,R),!,process(D, R, D2),!, repl(D2);
              write('Syntax error\n'),!, repl(D)
    ).

optParse([         ],Db,Db ).
optParse(['-t'|Args],Db,Db2) :- flag(trace,_,1), optParse(Args,Db,Db2).
optParse([   A|Args],Db,_  ) :- sub_atom(A,0,1,_,'-'), write("Usage: bjpl [-t] filename1 filename2 ...\n"), halt.
optParse([   A|Args],Db,Db2) :- consult1(atom(A),Db, Db1), optParse(Args, Db1, Db2).

main :-
  current_prolog_flag(argv, ARGV),
  flag(trace,_,0),
  consult1(atom('lib/initial.pl'),[], Db), % load files
  optParse(ARGV, Db, Db1),!,
  welcome(W),atom_length(W,L),
  (between(1, L, _), write(-), fail;nl),
  write(W),nl,
  (between(1, L, _), write(-), fail;nl),
  help,
  repl(Db1),halt.
