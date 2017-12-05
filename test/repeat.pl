:- assert(v(1)),repeat,v(N), writeln(N), N1 is N + 1,retract(v(N)),assert(v(N1)),N=5.
:- halt.
