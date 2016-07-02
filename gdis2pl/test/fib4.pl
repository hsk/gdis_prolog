fib(N, R)
	:-	((N = 0), (R = 0),!)
	;	((N = 1), (R = 1),!)
	;	((N1 is N - 1), (N2 is N - 2), fib(N1, R1),!, fib(N2, R2),!, R is R1 + R2,!).


:- fib(10,R),write(R),nl,halt.

