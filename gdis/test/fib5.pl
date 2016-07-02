fib(X0, A0) :- X0=0,A0=0.
fib(X1, A0) :- X1=1,A0=1.
fib(N, R) :- (N1 is N - 1), (N2 is N - 2), fib(N1, R1), fib(N2, R2), R is R1 + R2.
:- fib(10,R),write(R),nl,halt.
