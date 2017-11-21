:- op(1200,xfx,--).
term_expansion(A--B,B:-A).

!
--%--------- (Fib-0)
fib(0, 0).

!
--%--------- (Fib-1)
fib(1, 1).

Γ1 is Γ - 1,   Γ2 is Γ - 2,
fib(Γ1, R1),   fib(Γ2, R2),   R is R1 + R2
--%--------------------------------------- (Fib-N)
fib(Γ, R).

:- fib(10,R),write(R),write(α),nl,halt.
