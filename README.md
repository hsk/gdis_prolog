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

## 2017/11/17 version 0.0.2

- OCaml版を正式に採用することにしてディレクトリ配置を変更しました。
- TODOリストはissue listに移動させました。
- develop ブランチを切ってそちらで開発を進めてから、masterにmergeすることにしました。

## 2017/11/17 version 0.0.1 

- 久しぶりに更新。 integer/1 述語を追加しました。
- バージョン管理するようにしました。

# gdis2pl

Prologで作成したGDIS Prologです。

単純に移植したものから、Prologの単一化機能とデータ型を利用したものまで複数の実装があります。

# gdis2scala

GDIS PrologのScalaバージョンです。
