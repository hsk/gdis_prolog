add(A,B,[A|B]).

:- foldl(add,[1,2,3],[4],R),writeln(R),halt.
