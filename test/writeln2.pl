% 述語が終わっても何故かカットの影響が残っている。
writeln2(A) :- writeln(A),!.
name(test).
name(hoge).
:- name(A),writeln2(A),fail;true.
:- name(A),writeln(A),fail;true.
:- halt.
