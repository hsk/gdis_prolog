%test
a :- (a+b)+c,(c;b= ?).
a :- a+(b+c).
a :- (a*b)+(b*c)+a*b+b*c.
:- a.
:- b.
:- a,b.


m ::= %aaa
      true %g
    | pred(m) %b
    | iszero(m+a+c) %a
.

:- A=[],B=[1],C=[1,2],D=[1,2|3+1].

:- call((A=1;A=2,!)),A.
:- call(A=1),A.
