+++
date = "2018-09-21T18:58:53+09:00"
categories = ["emacs"]
tags = ["buffer"]
title = "Emacsの不要なバッファーを自動的にkillする"
+++

Emacsのbufferをtabで切り替えるのはあまり好きでないので switch-buffer の機能を使っているが、magitやdired関係の不要なバッファーが沢山増えていくと煩わしくなる。

手動で削除すれば済む話ではあるが、やっぱり自動化したい。MELPAでいろいろpackageを探したが適当なものがなく、ググっていたら下記の記事を見つけました。

<!--more-->

- [不要なバッファを自動的にkillしてくれる](https://www.shigemk2.com/entry/20120908/1347090453) 

早速設定して使ってみたところ実に快適！

```lisp
;; automatically kill unnecessary buffers
(use-package tempbuf)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'magit-mode-hook 'turn-on-tempbuf-mode)
```

- [tempbuf.el はここから落としてきました。](https://www.emacswiki.org/emacs/tempbuf.el) 

## 参考
switch-buffer機能は、ivy-switch-buffer と iflipb とを併用しています。

前者は、Helm-mini の代わりに使います。後者はキー操作によって開いているバッファーを切り替えたいときに使います。

```emacs-lisp
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(bind-key "M-:" 'ivy-switch-buffer)
```

iflipb.el の機能は、Emacs標準の ````M-x switch-to-next-buffer```` とは全く別物で優れものです。詳細はるびきちさんの記事が参考になるでしょう。

- [詳説！Alt+Tab感覚で賢くバッファをワンタッチで切り替える](http://emacs.rubikitch.com/iflipb/) 

```emacs-lisp
;; iflipb
(setq iflipb-ignore-buffers (list "^[*]" "^magit"))
(bind-key "C-<right>" 'iflipb-next-buffer)
(bind-key "C-<left>" 'iflipb-previous-buffer)
```
私は org-modeを使わないので ````C-<right>```` ````C-<left>```` に key bind してますが、そこはお好みで設定してください。

