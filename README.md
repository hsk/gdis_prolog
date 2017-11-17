# GDIS Prolog verion 0.0.1

GDIS PrologはGoals Database Index Stackマシンとして実装したPrologの単純なインタプリンタです。

OCaml 版以外に, Scala, Prologで実装したPrologもあります。

# Install

```
apt install ocaml
make
./gdispl test/eval3.pl
```

# Change history

## 2017/11/17 version 0.0.1 

久しぶりに更新。 integer/1 述語を追加しました。
バージョン管理するようにしました。

# todo

- [ ] `\` 演算子の実装
- [ ] opの実装
- [ ] リストの実装
- [ ] memberの実装
- [ ] maplistの実装
- [ ] callの実装
- [ ] foldlの実装
- [ ] DCGの実装
- [ ] Scala版のOCaml追従
- [ ] Prolog版のOCaml追従

## gdis2pl

Prologで作成したGDIS Prologです。

単純に移植したものから、Prologの単一化機能とデータ型を利用したものまで複数の実装があります。

## gdis2scala

GDIS PrologのScalaバージョンです。
