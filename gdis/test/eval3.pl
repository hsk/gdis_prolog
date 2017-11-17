mem(E,(E;E1)).
mem(E,(X;E1)) :- mem(E,E1).

mem(A=V,E)
------------- (Var)
eval(E,A,V)

--------------------------- (Abs)
eval(E,(X;A),(E;X;A))

eval(E,A,(E1;X;A_))    eval(E,B,B_)
eval((X=B_;E1),A_,A2)
-------------------------------------- (App)
eval(E,(A,B),A2)

integer(A) !
------------ (Integer)
eval(E,A,A)

eval(E,A,R1)    eval(E,B,R2)    R is R1 + R2
----------------------------------------- (Add)
eval(E,A+B,R)

eval(E,A,R1)    eval(E,B,R2)    R is R1 * R2
----------------------------------------- (Sub)
eval(E,A*B,R)

:- eval(0,(((a;b;a+b),1),2)*3,R),writeln(R),R=9,!,halt.
