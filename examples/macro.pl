:- op(1200,xfx,--).
term_expansion(A -- B, B :- A) :- writeln(macro1:(B:-A)).
writeln(aok) -- a.
:- a.
:- halt.
