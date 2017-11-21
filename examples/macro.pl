:- op(1200,xfx,--).
term_expansion(A--B,B:-A).

integer(I)
--%---------- (E-Int)
eval(I,I).

eval(E1,I1),   eval(E2,I2),   I is I1+I2
--%------------------------------------- (E-Int)
eval(E1+E2,I).

:- eval(1+2+3,R),writeln(R).
:- halt.
