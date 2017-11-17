# 標準ライブラリリファレンス

# 組み込み述語

# halt/0

# nop/0

# !/0

# ,/2

# ;/2

# =/2
# \=/2
# is/2
# assert/1
# write/1
# consult/1
# integer/1
# atom/1
# call/1...call/n

# 標準述語

# nl/1

example

    :- write(hello),nl,halt.

# writeln/1

example

    :- writeln(hello),halt.

# member/2

    member(A,[A|B]).
    member(A,[C|B]) :- member(A,B).

# maplist/2

example

    :- maplist(writeln,[1,2,3]),halt.

# maplist/3

example

    double(A,B) :- B is A * 2.
    :- maplist(double,[1,2,3],[2,4,6]),halt.

# maplist/4

example

    double_triple(A,B,C) :- B is A * 2, C is A * 3.
    :- maplist(double_triple,[1,2,3],[2,4,6],[3,6,9]),halt.
