add(A,B,C) :- C is A + B.

:- foldl(add,0,[1,2,3],R),writeln(R),halt.
