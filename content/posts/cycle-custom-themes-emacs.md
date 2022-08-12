+++
date = "2019-04-08T09:56:43+09:00"
categories = ["emacs"]
tags = ["theme"]
title = "Emacs：複数のテーマを切り替えて使う"
+++
Emacsを快適に使うためにいろいろ自分好みのテーマを模索します。私の場合、deeper-blue misterioso material と変遷し、今は doom-dracula を使っています。

それぞれ特徴がありコードを書く時、文章を書く時等々、その時々で気分転換を兼ねて切り替えて使えたら面白いと思ってググってみたところ下記サイトのQ&Aで面白いTipsを見つけたので試して見ました。結構快適で愉しいのでご紹介します。

* [stack-overflow](https://stackoverflow.com/questions/23793288/cycle-custom-themes-w-emacs-24)

## 設定 ##

``` emacs-lisp
(setq my-themes (list 'doom-dracula 'material 'misterioso 'deeper-blue)) 
(setq curr-theme my-themes)
(defun my-theme-cycle ()
  "Cycle custom theme."
    (interactive)
    (disable-theme (car curr-theme)) 
    (setq curr-theme (cdr curr-theme))
    (if (null curr-theme) (setq curr-theme my-themes))
    (load-theme (car curr-theme) t)
    (message "%s" (car curr-theme)))
(global-set-key [f7] 'my-theme-cycle)
(setq curr-theme my-themes)
(load-theme (car curr-theme) t)

```
