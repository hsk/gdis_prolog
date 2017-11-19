a(aname).
:- a(A),writeln(A),retract(a(AA)).
b(bname).
:- b(B),writeln(B).
:- asserta(c(cname)),c(C),writeln(C).
:- 1=2.
:- halt.
