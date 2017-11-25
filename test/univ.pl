:- a(b,c) =.. R, writeln(R).
:- a(1) =.. R, writeln(R).
:- 1+2 =.. R, writeln(R).
:- [] =.. R, writeln(R).
:- [1] =.. R, writeln(R).
:- [1,2] =.. R, writeln(R).
:- [1|2] =.. R, writeln(R).
:- [1,2,3,4|2] =.. R, writeln(R).
:- A = i,M = [A],L =.. [A|M], writeln(L).
:- A =.. B, writeln(B).
:- halt.

