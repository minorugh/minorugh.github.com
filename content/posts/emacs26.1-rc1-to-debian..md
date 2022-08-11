+++
date = "2018-04-28T16:18:57+09:00"
categories = ["emacs"]
tags = ["linux"]
title= "Debianにemacs-26.1-rc1をInstallする"
+++
私の場合は、ThinkpadのWindows10にVirtualBoxをInstallし、Vbox上でEmacs on Debianを動かしています。
もう一台メインマシンのmacbookにもEmacsを導入し、設定ファイルはDropboxで共用しています。

最近Macの方をemacs26.1にアップデートしたら、なぜかDebianのEmacs25.3で一部の設定ファイルが読み込みエラーになり起動しません。
やむなく、まだ安定版ではないけれどDebianのほうにもemacs26.1をインストールすることにしました。

## Installの手順
* apt-getでのパッケージインストールはまだ出来ないのでソースをダウンロードしてビルドします。
* ソースがすぐに見つからず探すのに苦労したがようやく下記にありました。

```shell
$ wget https://alpha.gnu.org/gnu/emacs/pretest/emacs-26.1-rc1.tar.xz
$ unar emacs-26.1-rc1.tar.xz
$ cd emacs-26.1
$ ./configure --with-x-toolkit=gtk3 --with-sound=no --with-x
$ make
$ sudo make install
$ sudo reboot 
$ emacs --version
$ GNU Emacs 26.1
…
```

* 今回 configure での ```--with-xwidgets``` オプションがエラーになりましたので諦めました。
* emacs26.1のインストールによって設定ファイル読み込みエラーは出なくなりました。原因は不明です。
