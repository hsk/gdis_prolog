# 標準ライブラリリファレンス

# 組み込み述語

# halt/0

  プログラムを終了する述語です。

# true/0

  何もしません。

# !/0

  カットです。

# '\\'/1

  述語の反転を行う前置演算子です。

  example

    :- A = 1, \(A = 2), writeln(A),halt.

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
# op/3

  ユーザー定義演算子を登録します。

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

# reverse/2

リストを反転させます。

# foldl/4

リストの畳込みを行います。

example

    add(A,B,C) :- C is A + B.
    :- foldl(add,0,[1,2,3],6),halt.

# forall/2

第一パラメータがtrueになるすべてのパターンで第二パラメータを実行します。

example

    name(test).
    name(hoge).
    name(fuga).
    :- forall(name(A),writeln(A)).
    :- halt.

# findall/3

第二パラメータがtrueになるすべてのパターンを取り出し第一パラメータのリストとして第三パラメータに返します。

example

    name(test).
    name(hoge).
    name(fuga).
    :- findall(A,name(A),R),maplist(writeln,R).
    :- halt.
