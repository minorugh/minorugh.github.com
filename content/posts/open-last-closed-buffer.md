+++
date = "2021-09-17T14:24:33+09:00"
categories = ["emacs"]
tags = ["elisp"]
title = "Emacs : Open Last Closed Buffer"
+++

darkroom-modeを使うときは、Syntax Highlightが邪魔なので、どのモードのファイルであっても強制的に text-modeで開くように設定している。
当然ながら darkuroom-modeから抜けるときにはもとのモードに戻す必要がある。

もとの major-modeを記憶させておいて restoreすればいいのだが、
auto-save-buffers-enhanced で自動保存しているので、回りくどいことをしなくても一度 Kill-bufferして再び読み込めばよい。

いろいろググって下記の Tipsを見つけた。

* [Emacs: Open Last Closed File](https://getpocket.com/read/1325332326) 

原案では、多機能で長いコードになっていましたが、私には必要のない部分は削って dietしました。

```emacs-lisp
;; Open last closed file
(defvar my:recently-closed-buffers nil)

(defun my:close-current-buffer ()
  "Close the current buffer."
  (interactive)
  (setq my:recently-closed-buffers
		(cons (cons (buffer-name) (buffer-file-name)) my:recently-closed-buffers))
  (kill-buffer (current-buffer)))

(defun my:open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop my:recently-closed-buffers))))
```
ようは、`kill-buffer` の代りに `my:close-current-buffer` を使い、再読込したいときに `my:open-last-closed` すればよいことになる。

darkroom-modeの in/out 設定は以下のようになった。

```emacs-lisp
(leaf darkroom
  :ensure t
  :bind ("<f12>" . my:darkroom-in)
  :config
  (bind-key "<f12>" 'my:darkroom-out darkroom-mode-map)

  (defun my:darkroom-in ()
	(interactive)
	(display-line-numbers-mode 0)
	(setq line-spacing 0.4)
	(darkroom-tentative-mode 1))


  (defun my:darkroom-out ()
	(interactive)
	(darkroom-tentative-mode 0)
	(setq line-spacing 0.1)
	(display-line-numbers-mode 1)
	(my:close-current-buffer)
	(my:open-last-closed)))
```

つまり、darkroom-outのとき、一度 bufferを閉じて（kill-buffer）、再度読み込むという処理をしているわけである。
