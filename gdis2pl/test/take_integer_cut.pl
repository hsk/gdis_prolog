take_integer([X | Xs], Ys) :-
    take_integer(X, Ys1), take_integer(Xs, Ys2), append(Ys1, Ys2, Ys), !.
take_integer(X, [X]) :- integer(X), !.
take_integer(X, []).

:- take_integer([1, a, [2, b]], X),write(X),write('\n'),halt.
