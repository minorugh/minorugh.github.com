+++
date = "2019-03-28T16:34:06+09:00"
categories = ["emacs"]
tags = ["buffer"]
title = "Emacsのウインドウ操作の履歴をUndo/Redoする：winner-mode"
+++
emacsでいろいろ作業していると意図しない形で勝手にウインドウズが分割されたりします。設定して "C-g" で隠せるものもありますが、それも出来ないときは、"C-x 0" や "C-x 1" のお世話になります。


``` emacs-lisp
(winner-mode)
(bind-key "C-<left>" 'winner-undo)
(bind-key "C-<right>" 'winner-redo)
```

``` emacs-lisp
(winner-mode)
(bind-key "C-c <left>" 'winner-undo)
(bind-key "C-c <right>" 'winner-redo)
```
