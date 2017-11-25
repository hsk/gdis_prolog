:- a(b,c) =.. R, writeln(R).
:- a(1) =.. R, writeln(R).
:- 1+2 =.. R, writeln(R).
:- [] =.. R, writeln(R).
:- [1] =.. R, writeln(R).
:- [1,2] =.. R, writeln(R).
:- [1|2] =.. R, writeln(R).
:- [1,2,3,4|2] =.. R, writeln(R).
:- A =.. B, writeln(B).
:- halt.

