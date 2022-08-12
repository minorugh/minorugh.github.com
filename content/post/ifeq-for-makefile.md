+++
date = "2018-10-15T13:13:43+09:00"
categories = ["tech"]
tags = ["makefile"]
title = "makefile でのOS条件分岐"
+++
自動化のためにmakefileを書いています。

私の場合、Linux端末とMacとの異なる環境で共通のmakefileを使いますので、条件分岐を勉強してみました。無手勝流ながら以下の設定でうまく作動してくれるようになりました。
<!--more-->

```shell
# maikefile

ifeq ($(shell uname),Linux)
.txt.html:
	perl ~/Dropbox/Web/GH/mw/makeweb.pl $< $@
	chromium $@
else
.txt.html:
	perl ~/Dropbox/Web/GH/mw/makeweb.pl $< $@
	open -a "Google Chrome" $@
endif
```

#### ifeq を使う

```shell
ifeq  ($(shell uname),Linux)
``` 

というところが条件判断の部分です。

何をしているかというと、[結城浩さんのMakewebというPerlスクリプト](https://www.hyuki.com/makeweb/) を使って、TEXTフォーマットのファイルをHTMLフォーマットに書き出しています。この部分はLinuxもMacも共通です。


つづいて、書き出された当該ファイルを自動的に Chromeで開くという簡単なコマンドなのですが、LinuxとMacとでは命令の標記が異なるので分岐させて各々のコマンドを書いています。

#### uname
unameは、Linuxコマンドなので、windowsでは使えないですが、Linux/Mac の判別には便利です。

ターミナルで ````uname```` コマンドを打つことで、OSの情報を得ることができます。

上記の例では、Linux かどうかを判断させてLinuxのコマンドを書き、elseでMac のコマンドを実行させていますが、逆にしたい場合は以下のように書きます。


```shell
ifeq  ($(shell uname),Darwin)
```
