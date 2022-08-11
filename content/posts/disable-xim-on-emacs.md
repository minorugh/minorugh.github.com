+++
date = "2020-09-09T21:18:55+09:00"
categories = ["emacs"]
tags = [""]
title = "EmacsのときにXIMを無効化する"
+++

emacs-mozcを使っているときは、紛らわしいのでfcitxをOFFにしておきたい…と、あれこれ悩んでいたのですが、「Emacs利用時にはXIMを無効にする」というのはが常識だったようで今更ながら ~/.Xresources を設定しました。これを設定すると何故かEmacsの起動も若干早くなる。不思議


``` bash
!~/.Xresources
! Emacs XIMを無効化
Emacs*useXIM: false

```

私の場合、fcitxのON/OFFをWindowsにあわせて `hiragana-katakana` に設定しているのですが、
上記を設定することでEmacs利用時にはこのキーが空いてくるので、emacs-mozcのON/OFFも `hiragana-katakana` に割り当てることにしました。

ようするに、emacs利用時でもそうでないときでも同じキーでFEPのON/OFFが可能となりました。



## 参考

- http://linux.ikoinoba.net/index.php?UID=1336059496
- https://www.gnu.org/software/emacs/manual/html_node/emacs/Table-of-Resources.html#Table-of-Resources

