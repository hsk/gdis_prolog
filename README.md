# GDIS Prolog verion 0.1.2

GDIS Prolog はコンパクトであることを目標にした Prolog インタプリンタです。

GDIS とは Goals Database Index Stack の略です。
インタプリタはゴール、データベース、インデックス、スタックの状態マシンとして構築されています。

実装は非常にコンパクトですが、小さいながらも、インストールして気軽に使うことが出来ます。

他の言語の作成にコントリビューションする際の、ドキュメントの整備や、貢献の仕方なども分かりやすくまとめたいと思います。

VSCode プラグインを作るのも目標の１つです。簡単なPrologのプログラムをデバッグできれば便利なはずです。

## 特徴

- 小さなプログラム
- OCaml による綺麗な実装
- OCamlYacc による構文解析と下降型演算子順位法によるユーザー定義演算子

# Install

Ubuntu 17.04

```bash
sudo add-apt-repository ppa:h-sakurai/gdis-prolog
sudo apt-get update
sudo apt install gdispl
```

source build

```bash
apt install ocaml
make
sudo make install
```

# Hello world

examples/hello.pl

```prolog
:- writeln('hello world'),halt.
```

```bash
gdispl examples/hello.pl
hello world
```

# Uninstall

```
sudo make uninstall
```

# Change history

## 2017/11/21 version 0.1.2

- ユーザー定義演算子をop/3 で以下のように定義して使うことができるようになりました:

    ```prolog
    :- op(600,xfy,add).
    :- op(500,xfy,mul).

    eval(I,I) :- integer(I).
    eval(A add B, I) :- eval(A,A_),eval(B,B_), I is A_ + B_. 
    eval(A mul B, I) :- eval(A,A_),eval(B,B_), I is A_ * B_. 
    :- eval(1 mul 2 add 3 add 4 mul 5,I),writeln(I),halt.
    ```

    以下のコマンドで試してみることが出来ます:

    ```bash
    $ gdispl examples/op.pl
    25
    ```

- -v オプションで以下のようにバージョン情報の表示をするようになりました:

    ```bash
    $ gdispl -v
    GDIS Prolog version 0.1.2
    ```
- --help オプションのヘルプ表示は以下のようにアラインを合わせて綺麗に表示するようになりました:

    ```bash
    $ gdispl --help
    Usage: gdispl [options] file...
    Options:
      -t            Set trace mode
      -v            Display version infomation
      -lib libpath  Set libraries path
      -help         Display this list of options
      --help        Display this list of options
    ```

## 2017/11/21 version 0.1.1

- 演算子の優先順位を変更可能にするために独自拡張機能を一旦なくしました。
- 構文解析は1文ごとに処理して、リストを返さなくしました。
- パーサは演算子の組み換えができる構造に変わりました。

## 2017/11/20 version 0.1.0

Ubuntu 17.04 で以下のコマンドでインストールできるようになりました。

```bash
sudo add-apt-repository ppa:h-sakurai/gdis-prolog
sudo apt-get update
sudo apt install gdispl
```

## 2017/11/20 version 0.0.9

- PPA 登録テスト

## 2017/11/20 version 0.0.8

- PPA 登録テスト

## 2017/11/20 version 0.0.7

- 複数の:-/1述語で問題なく動作するように
- nop/0 を true/0 に変更
- forall/2 を追加
- reverse/2 を追加
- retract/1 を追加
- asserta/1 を追加
- retractall/1 を追加
- リスト表示バグ修正

## 2017/11/18 version 0.0.6

- '\\'/1 を追加

## 2017/11/17 version 0.0.5

- リストのライブラリを追加
  - member/2 を追加
  - call/1 を追加
  - maplist/2,maplist/3,maplist/4 を追加
  - foldl/4 を追加
- [GDIS Prolog 仕様](https://github.com/hsk/gdis_prolog/blob/master/docs/README.md) を追加
- [ライブラリリファレンス](https://github.com/hsk/gdis_prolog/blob/master/docs/library.md) を追加

## 2017/11/17 version 0.0.4

- README を修正
- examples/eval3.pl を修正
- examples/lambda.pl を追加
- docs/README.md にBNF定義を追加
- testファイルを追加
- 空白行が続いた時の構文解析の仕様変更

## 2017/11/17 version 0.0.3

- README を修正
- Scala版とProlog版をlabディレクトリに移動
- test ディレクトリ名を examples に変更
- examples/eval3.pl を修正

## 2017/11/17 version 0.0.2

- make install make uninstall を追加しました。
- OCaml版を正式に採用することにしてディレクトリ配置を変更しました。
- TODOリストはissue listに移動させました。
- develop ブランチを切ってそちらで開発を進めてから、masterにmergeすることにしました。

## 2017/11/17 version 0.0.1 

- 久しぶりに更新。 integer/1 述語を追加しました。
- バージョン管理するようにしました。

## ライセンス

MIT License
