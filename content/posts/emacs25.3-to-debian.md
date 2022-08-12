+++
date = "2017-09-25T18:41:37+09:00"
categories = ["emacs"]
tags = ["linux"]
title= "DebianにEmacs25.3をInstallする"
+++
私の場合は、ThinkpadのWindows10にVirtualBoxをInstallし、Vbox上でDebianを動かしています。

## Installの手順
- apt-getでインストールできるのは、Emacs25.1までなので、dawnloadしたsourceを直接コンパイルしてインストールします。
- 幸い、もともとEmacs25.2がInstallしたあったので、すんなり成功しました。

```shell
$ wget http://public.p-knowledge.co.jp/gnu-mirror/emacs/emacs-25.3.tar.xz 
$ unar emacs-25.3.tar.xz
$ cd emacs-25.3
$ ./configure --with-x-toolkit=gtk3 --with-sound=no --with-x --with-xwidgets
$ make
$ sudo make install
$ sudo reboot 
$ emacs --version
$ GNU Emacs 25.3.1
…
```

- emacs25.2はインストールしたままで上書きしましたが、まったく問題ありませんでした。

## xwidgetsが動いた(^o^)v
- Emacs25で採用されたxwidgetsですが、Mac環境では動きませんでした。
- Linuxでは動くとの情報だったので試しました。


