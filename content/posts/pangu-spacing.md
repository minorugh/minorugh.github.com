+++
date = "2021-11-18T08:41:01+09:00"
categories = ["emacs"]
tags = ["elisp"]
title = "pangu-spacing.el : 全角と半角の間に自動でスペースを入れる"

+++

* [pangu-spacing.el : 全角と半角の間に自動でスペースを入れる新実装](http://emacs.rubikitch.com/pangu-spacing/) 

上記の Tipsを参考にして、markdown-modeに導入してみた。

日本語においては、いわゆる半角文字と全角文字の間にスペースを入れた方が見やすいと言われていて、`pangu-spacing.el` はそれを自動で行ってくれるというものです。

ただ、「Google日本語入力」などと書くときに「Google 日本語入力」ではおかしくなるし、なんとなく間延びした感じになるのが好みではなかったので
アルファベット文字列の右端だけにスペースが入るようにカスタマイズした。

`pangu-spacing.el`のコードを見るとデフォルトでは、正規表現で [a-zA-Z0-9] なっているので半角数字の場合にも同様に処理されてしまう。
数字にも半角スペースが有効になると、[2021 年 11 月 16 日] という感じになるので、日付表示のケースでは面白くない。
そこで、正規表現の記述を [a-zA-Z] に変更して半角数字は対象外としアルファベットのみが pangu-spacingの対象となるようにしている。

```emacs-lisp
(leaf pangu-spacing
  :ensure t
  :hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp
		(rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
						 (group-n 1 (or (category japanese))))))
			(group-n 2 (in "a-zA-Z")))))
```

上記の例では、markdown-mode と text-modeに導入している。

