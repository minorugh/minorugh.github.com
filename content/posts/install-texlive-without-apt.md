+++
date = "2020-02-10T09:09:09+09:00"
categories = ["tech"]
tags = ["Latex","linux"]
title = "apt installを使わずにTeXLiveを導入する"

+++

## TeX ディストリビューション
Linux で TeX 環境を構築するには，2つの方法があります．

1. 使用している Linux ディストリビューションのパッケージ管理システムから TeX Live のパッケージをインストールする．
2. TeX Live のインストーラを使ってインストールする．
 
- https://texwiki.texjp.org/?Linux

前者の場合は，他のパッケージと同様に統一的な管理ができますが，ディストリビューションによっては提供されているパッケージのバージョンが古いことがあります． 後者の場合は，パッケージ管理システムによる管理からは外れてしまいますが，tlmgr を使って最新の状態にアップデートし続けることが可能です．

## TeX Live のインストール †

TeX Live のインストールガイド

- http://www.tug.org/texlive/quickinstall.html
- http://www.tug.org/texlive/doc/texlive-en/texlive-en.html#installation
- http://www.tug.org/texlive/doc/texlive-ja/texlive-ja.pdf#c

に従えばよいですが，このページでもネットワークインストーラを使う場合について簡単に説明します．

まず，ミラーサイトから install-tl-unx.tar.gz をダウンロードします．

### ※ wget を使用する場合

```shell
$ wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
```

### ※ curl を使用する場合

```shell
$ curl -O http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
```

install-tl-unx.tar.gz を展開します．

```shell
$ tar xvf install-tl-unx.tar.gz
```

展開したインストーラのディレクトリに移動します．

```shell
$ cd install-tl*
```

root 権限でインストーラを実行します． オプションでダウンロードするリポジトリを指定できます．

```shell
$ sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/

...
Actions:
 <I> start installation to hard disk
 <H> help
 <Q> quit
Enter command: I
```

I を入力してインストールを開始します． サーバーの接続エラーが発生したり，何らかの理由により取得したアーカイブに問題があったりした場合はインストールが途中でストップします． この場合は，以下のコマンドで途中から再開できたりできなかったりします．

```shell
$ sudo ./install-tl -no-gui -profile installation.profile

ABORTED INSTALLATION FOUND: installation.profile
Do you want to continue with the exact same settings as before (y/N): y
```

再開できない場合は接続先を変更するか，または ISO ファイルをミラーサイトからダウンロードしてインストールしてください．

インストールが終了したら /usr/local/bin ディレクトリ配下にシンボリックリンクを追加します．

```shell
$ sudo /usr/local/texlive/????/bin/*/tlmgr path add
```

### アップデート †

アップデートは

```shell
$ sudo tlmgr update --self --all
```

を実行すれば OK です．

ただし，アップデートのタイミングによっては，今まで動いていたものが動かなくなったりすることがあるかもしれません．

- /usr/local/texlive/????/tlpkg/backups

にパッケージのバックアップが保存されています． アップデートによって動作しなくなった場合は

```shell
$ sudo tlmgr restore （パッケージ名） （リビジョン番号）
```

とすることで以前のバージョンに戻すことができます．


### ※ My-styを追加する場所

```shell
 /usr/local/texlive/????/texmf-dist/tex/platex
```

Dropboxからシンボリックを貼る

```shell
sudo ln -s /home/minoru/Dropox/dotfiles/tex/platex/my-sty /usr/local/texlive/????/texmf-dist/tex/platex 
```
そのあと、`sudo mktexlsr` する。

