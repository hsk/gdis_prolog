# GDIS Prolog Interpretor

Implemented GDIS Prolog in Prolog.
GDIS Prolog used PAIP as reference.
However, GDIS model is GDIS machine, that look like SECD machine.
GDIS machine is an abbreviation for Goals Database Index Stack machine.

- Goals is Goal stack, when first elements size is one.
- Database is Prolog Term list.
- Index is Pointer for Database.
- Stack is stack data of Goals, unification substitution Environment and Index.

## Feature

- :- prefix operator :
- =/2 infix operator : unification
- ,/2 infix operator : conjunction
- ;/2 infix operator : 'or' predicate
- is/2 infix operator : expression evalutes
- assert/1 predicate : add term to database
- consult/1 predicate : load of file
- halt/0 predicate : exit program
- write/1 preicate : stdout output
- !/0 predicate : cut

## Constitution

- gdis.pl カットはあるがスタックは増え続ける。
- gdis2.pl カットでスタックを消せるようにスタック長を別に持つ事にした。
- gdis3.pl prologのデータ構造をそのまま使いたいので、実験。
- gdis4.pl prologのデータ構造をそのまま使い、単一化もPrologの機能を使った。
- gdis5.pl 全ての組み込み関数をデータベースに入れる。
- gdis6.pl インデックスを命令リストにしてより抽象化する。
- gdis7.pl 組み込み述語の登録関数を作って短くする。


## 考察

PrologによるPrologの実装では型やパーサを定義せずに実装出来るため200行程度で短く記述出来ました。
また、例外処理的なfailを使う事でOption型を使ったような記述を短く書く事が出来ました。

しかし、OCaml版をScalaに移植するだけなら1日あれば出来るところ、Prologへの移植は数日かかかりました。
大きな原因は３つ程考えられます。原因の１つは関数型言語と論理型言語のギャップです。Prologの述語は関数のように値を返す事は出来ないので書き換えが結構手間でした。
２つ目の理由はProlog自体に慣れていない事です。Scalaに移植する場合は、大体使う関数は理解しているので楽に移植出来ますが、Prologの述語は知らない事が多いので時間がかかりました。
３つ目の理由はPrologには静的型検査がない事です。シンタックスエラーを無くすまではいいのですが、シンタックスエラーがなくなった後のバグ取りにかなり時間がかかりました。動かしてみながらのデバッグはtrace機能を使うとブレークポイントを使ったようなデバッグが可能ですがそれでも大変でした。

特に３つ目の型がないが故の移植の困難さはかなり大きな物がありました。
言語の初心者は特にノウハウがないので、初めて使う機能で間違える事は多いのです。
言語機能に慣れてくれば、余計な型検査はなくても問題は発見しやすくなるのでしょうが、言語機能を知らない事と型検査機能がない事は相乗効果的に開発効率を落とします。
初心者にとって型検査はバグを発見するよい手段です。

静的型検査、あるいは型推論のある論理型言語があるとよいのではないでしょうか？

## 今後

今回の実装はOCaml版を移植しただけなので、実装はもっと簡潔に出来るかもしれません。
例えば、Prologの型をそのまま使う、単一化をPrologの単一化で記述する、success,failというような記述を無くす事が考えられます。

また、実装したPrologでさらにPrologを実装出来る、いわゆるメタサーキュラーなインタプリタに出来るとよいでしょう。

論理型言語で論理型言語を実装したので、より整理する事で、形式的に記述する事が出来るでしょう。


- `cut2.pl`でokと表示されるようにする。
- 演算子定義可能にする。
- listを使えるようにする。
- `take_integer`が動くかどうか調べる。
- 操作的意味論などの論文をもっと調べる。

## Refarence

- [prolog.lisp](http://norvig.com/paip/prolog.lisp) of [PAIP (Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp)](http://norvig.com/paip).
