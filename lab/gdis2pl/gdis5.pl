#!/usr/bin/env swipl --toplevel=halt --stand_alone=true -q
:- initialization(main).
:- style_check(-singleton).

% prolog

pop((_, D, _, [(G, I)|S]), succ(G, D, I, S)).
pop((_, D, _,         []), fail(D)).

uni(M,T,T,M2) :- M=([_|G],D,_,[(Sg,I)|S]), M2=succ(G, D, 0,[(Sg,I)|S]).
uni(M,_,_,M2) :- pop(M,M2).

assert1(':-'(T),D, R) :- process(D, [], T, R).
assert1(T      ,D, R) :- append(D, [T], R).

read_stream_to_terms(Stream, RC, REnv) :-
  read_term(Stream, C, [variable_names(Env)]),!,
  (C=end_of_file,
    RC=[], REnv=[],!
  ; read_stream_to_terms(Stream, RC1, REnv1),
    RC=[C|RC1], append(Env,REnv1,REnv),!
  ).

cnv2(A :- B, A :- B).
cnv2(:- B, :- B).
cnv2(T, T :- nop).

consult1(Filename,D,D2) :-
  (flag(trace,Tr,Tr),Tr=1,write("Loading "),write(Filename),nl;!),!,
  setup_call_cleanup(
    open(Filename, read, In),
    read_stream_to_terms(In, Terms, Env),
    close(In)
  ),
  maplist(cnv2,Terms,Terms2),!,
  foldl(assert1, Terms2, D, D2),!.

step(succ(G,D,I,S), R2) :- (flag(trace,Tr,Tr),Tr=1,format("i=~d G=~p S=~p~n",[I,G,S]);!),!,step1((G,D,I,S), G,D,I,S, R2).
step(R, R).

step1(M,           [ ], D,  I, S, R) :- R=succ([], D,  I, S),!.
step1(M,         [T|G], D,  I, S, R) :- nth0(I, D, (T1=T2)), copy_term(T1,T11),T11=T,!,call(T2,M,M,R).
step1(M,         [T|G], D,  I, S, R) :- nth0(I, D, T_),copy_term(T_,':-'(T2,T3)),I1 is I + 1, T = T2,!, step(succ([T3|G], D, 0, [([T|G], I1)|S]),R).
step1(M,         [T|G], D,  I, S, R) :- nth0(I, D, T_),I1 is I + 1, step(succ([T |G], D, I1, S), R).
step1(M,            G , D,  I, S, R) :- pop(M,M2), step(M2,R).

builtin_halt(   M,(            G , D, I, S), R) :- halt.
builtin_fail(   M,(            G , D, I, S), R) :- fail(D).
builtin_nop(    M,([         _|G], D, I, S), R) :- step(succ(G, D, 0, S), R).
builtin_cut(    M,([         _|G], D, I, S), R) :- step(succ(G, D, 0, [([fail], 0)]), R).
builtin_comma(  M,([(U,V)     |G], D, I, S), R) :- step(succ([U,V|G], D, 0, S), R).
builtin_semi(   M,([(U;V)     |G], D, I, S), R) :- step(succ([U|G], D, 0, [([V|G], 0)|S]), R).
builtin_eq(     M,([(U=V)     |G], D, I, S), R) :- uni(M,U,V,R1),step(R1, R).
builtin_is(     M,([(U is V)  |G], D, I, S), R) :- N is V, !, uni(M,U,N,R1), step(R1,R).
builtin_assert( M,([ assert(T)|_], D, I, S), R) :- assert1(T, D, D2), step(succ(G, D2, I, S), R).
builtin_write(  M,([  write(T)|G], D, I, S), R) :- write(T), step(succ(G, D, 0, S), R).
builtin_consult(M,([consult(T)|_], D, I, S), R) :- consult1(T, D, D2), step(succ(G, D2, I, S),R).

inidb(Db) :-
  Db = [halt=builtin_halt,
        nop=builtin_nop,
        (!) = builtin_cut,
        (_,_) = builtin_comma,
        (_;_) = builtin_semi,
        (_=_) = builtin_eq,
        (_ is _) = builtin_is,
        assert(_) = builtin_assert,
        write(_) = builtin_write,
        consult(_) = builtin_consult
 ].

solve(M, R2) :- ([], _, _, _) = M, pop(M, R), step(R, R2).
solve(M, R2) :- ( G, D, I, S) = M, step(succ(G,D,I,S), R2).

read_line(P, A) :- prompt1(P), read_line_to_codes(user_input,R),atom_codes(A,R).

env_show_add_(K=V,R) :- format(atom(R),"~s=~p",[K,V]).
env_show(Vs,R) :- maplist(env_show_add_,Vs,E3), atomic_list_concat(E3,', ',R).

prove(Env,M,D2) :- solve(M, R),
  ( R = fail(D2)
  ; R = succ(G, D, I, S),
    env_show(Env, X), write(X),nl,
    ((S=[]; S=[(fail,0)]), write("Yes.\n"), D2=D
    ; (read_line("More y/n", "y"), prove(Vs,(G, D, I, S), D2)
      ; D2=D
      )
    )
  ).

process(D, Env,T, D2) :- prove(Env,([T], D, 0, [([fail],0)]), D2).

% main

welcome("Beautiful Japanese Prolog Interpreter").

help :- maplist(format('~s\t~s\n'),[["e","exit"], ["l","list"],["h","help"]]).

syntax_print(T) :- write(T),write('.'),nl.

repl(D) :-
    read_line("? ", Y),!,
    (Y='e'
    ;Y='l', maplist(syntax_print, D),!, repl(D)
    ;Y='h', help,!, repl(D)
    ;Y='t', flag(trace,Trace,1-Trace),write('Tracing '),
            (Trace=0,write('on');write('off')),nl,!,repl(D)
    ;         term_string(R,Y,[variable_names(Env)]),!,process(D,Env, R, D2),!, repl(D2);
              write('Syntax error\n'),!, repl(D)
    ).

optParse([         ],Db,Db ).
optParse(['-t'|Args],Db,Db2) :- flag(trace,_,1), optParse(Args,Db,Db2).
optParse([   A|Args],Db,_  ) :- sub_atom(A,0,1,_,'-'), write("Usage: bjpl [-t] filename1 filename2 ...\n"), halt.
optParse([   A|Args],Db,Db2) :- consult1(A,Db, Db1), optParse(Args, Db1, Db2).



main :-
  current_prolog_flag(argv, ARGV),
  flag(trace,_,0),
  inidb(Inidb),
  consult1('lib/initial.pl',Inidb, Db), % load files
  optParse(ARGV, Db, Db1),!,
  welcome(W),atom_length(W,L),
  (between(1, L, _), write(-), fail;nl),
  write(W),nl,
  (between(1, L, _), write(-), fail;nl),
  help,
  repl(Db1),halt.
