+++
date = "2018-01-23T20:12:25+09:00"
categories = ["hugo"]
tags = ["linux"]
title= "Debian９にHUGO最新バージョンをインストールする"
+++
## はじめに
Emacs+HUGOをMac環境メインで使っているが、Windows10（ThinkPad）上のVboxにdebianを入れて予備環境も構築している。

MacでのHUGOは、brewで簡単にバージョンアップできるのだけれど、
Linuxの場合、apt-getをupdateしても古いバージョンのHUGOしかインストール出来ない。
そこで、直接最新版をダウンロードしてインストールしたので備忘録としてメモしておく。

## HUGO最新版をダウンロードしてインストール

端末で直接ダウンロードすることも出来るが、どのみち最新版を調べる必要があるので,
プラウザでダウンロードサイトにアクセスする。

https://github.com/spf13/hugo/releases/

リストが表示されるので、最新バージョンを落としてくる。

hugo_0.34_Linux-64bit.deb

使っている環境に合うものをダウンロードする。

普通に操作すれば、Downloadsフォルダに保存されるので、あとはターミナルからコマンドを使ってインストールする。

```shell
$ cd ~/Downloads
$ sudo dpkg -i hugo_0.34_Linux-64bit.deb

```

終わったら、念のためにHUGOのバージョンを確認しておこう。

```
$ hugo version
Hugo Static Site Generator v0.34 linux/amd64 BuildDate: 2018-01-22T12:06:40Z

```


Conglaturation…
