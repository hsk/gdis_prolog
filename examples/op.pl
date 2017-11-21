:- op(600,xfy,add).
:- op(500,xfy,mul).

eval(I,I) :- integer(I).
eval(A add B, I) :- eval(A,A_),eval(B,B_), I is A_ + B_. 
eval(A mul B, I) :- eval(A,A_),eval(B,B_), I is A_ * B_. 
:- eval(1 mul 2 add 3 add 4 mul 5,I),writeln(I),halt.
