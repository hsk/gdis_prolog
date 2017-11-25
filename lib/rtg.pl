% rtg.pl : Regular Tree Grammer validator generator

:- op(1200, xfx, [::=]).

term_expansion(A::= syntax, syntax(A)).
term_expansion(A::=B, A_ :- syntax(A_,M,B)) :- assert(syntax(A)), apply_expansion(A,[M],A_).

%goal_expansion(syntax(A_,M,A),B_) :- !,syntax_expansion(M,A,B_), writeln(A_ :- B_).
goal_expansion(syntax(_,M,A),B_) :- syntax_expansion(M,A,B_).

apply_expansion(A,M,L) :- atom(A), L =.. [A|M].
apply_expansion(A,M,L) :- A=..B,append(B,M,R), L =.. R.

%syntax_expansion(M,A,R) :- writeln(sn;a:A),fail.
syntax_expansion(M,A,R) :- var(A),apply_expansion(call,[A,M],R).
syntax_expansion(M,B|Bs,(B_,!);Bs_) :- syntax_expansion(M,B,B_),!, syntax_expansion(M,Bs,Bs_).
syntax_expansion(M,escape(B|Bs),(M=(M1|M2),B_,Bs_)) :- syntax_expansion(M1,B,B_),syntax_expansion(M2,Bs,Bs_).
syntax_expansion(M,A,R)   :- syntax(A),!,apply_expansion(A,[M],R).
syntax_expansion(M,A,M=A) :- atom(A),!.
syntax_expansion(M,A,(M=B,!,B_))  :- A =.. [A_|Ps], maplist(syntax_expansion,Ms,Ps,Es), B =.. [A_|Ms],!,
                                     reverse(Es,Es1),!,
                                     foldl(syntax_expansion1,Es1,!,B_).
syntax_expansion1(A1,B1,(A1,B1)).
:- discontiguous(syntax/1).
